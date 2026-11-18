import glob
import json
import os
import shutil
import subprocess
import uuid
from pathlib import Path
from typing import List

from fastapi import FastAPI, File, Form, HTTPException, UploadFile
from fastapi.responses import FileResponse, HTMLResponse, JSONResponse

BASE_DIR = Path(__file__).resolve().parent
JOBS_DIR = BASE_DIR / "jobs"
JOBS_DIR.mkdir(exist_ok=True)
LISP_PATH = BASE_DIR / "robot_areas_batch.lsp"

app = FastAPI(title="Portal de Areas y PDF - AutoCAD")


def find_accoreconsole() -> str:
    env_path = os.environ.get("AUTOCAD_CORE_PATH", "").strip()
    if env_path and Path(env_path).is_file():
        return env_path
    patterns = [
        r"C:\Program Files\Autodesk\AutoCAD *\accoreconsole.exe",
        r"C:\Program Files\Autodesk\AutoCAD*\accoreconsole.exe",
    ]
    matches: List[str] = []
    for pat in patterns:
        matches.extend(glob.glob(pat))
    if matches:
        matches.sort(reverse=True)
        return matches[0]
    raise FileNotFoundError(
        "No se encontro accoreconsole.exe. Define la variable de entorno AUTOCAD_CORE_PATH."
    )


@app.get("/", response_class=HTMLResponse)
async def home():
    html_file = BASE_DIR / "index.html"
    return HTMLResponse(content=html_file.read_text(encoding="utf-8"))


@app.post("/api/procesar")
async def procesar_plano(
    archivo: UploadFile = File(...),
    zonas_extra: str = Form("[]"),
):
    if not archivo.filename or not archivo.filename.lower().endswith(".dwg"):
        raise HTTPException(status_code=400, detail="Solo se admiten archivos .dwg")

    try:
        core_exe = find_accoreconsole()
    except FileNotFoundError as exc:
        raise HTTPException(status_code=500, detail=str(exc))

    job_id = uuid.uuid4().hex[:10]
    job_dir = JOBS_DIR / job_id
    job_dir.mkdir(parents=True, exist_ok=True)

    safe_name = Path(archivo.filename).name
    dwg_path = job_dir / safe_name
    with open(dwg_path, "wb") as out:
        shutil.copyfileobj(archivo.file, out)

    # Guardar zonas personalizadas si el usuario las capturo en la web
    try:
        zonas_list = json.loads(zonas_extra) if zonas_extra else []
    except json.JSONDecodeError:
        zonas_list = []

    if zonas_list:
        extra_txt = job_dir / "extra_zones.txt"
        lines = []
        for z in zonas_list:
            nombre = str(z.get("nombre", "")).strip().replace("|", "-")
            color = int(z.get("color", 7))
            if nombre and 1 <= color <= 255:
                lines.append(f"{nombre}|{color}")
        if lines:
            extra_txt.write_text("\n".join(lines) + "\n", encoding="utf-8")

    # Copiar LISP al directorio del job para evitar problemas de Trusted Paths/espacios
    job_lsp = job_dir / "robot_areas_batch.lsp"
    shutil.copyfile(LISP_PATH, job_lsp)

    lisp_posix = str(job_lsp).replace("\\", "/")
    scr_path = job_dir / "run_batch.scr"
    scr_content = (
        f'(load "{lisp_posix}")\n'
        "ROBOTAREAS_BATCH\n"
        "_QSAVE\n"
        "_QUIT\n"
    )
    scr_path.write_text(scr_content, encoding="utf-8")

    cmd = [core_exe, "/i", str(dwg_path), "/s", str(scr_path), "/l", "es-ES"]
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(job_dir),
            capture_output=True,
            text=True,
            timeout=180,
        )
    except subprocess.TimeoutExpired:
        raise HTTPException(status_code=504, detail="Tiempo de espera agotado al procesar el DWG.")

    pdf_name = f"{dwg_path.stem}_AREAS.pdf"
    pdf_path = job_dir / pdf_name
    if not pdf_path.is_file():
        return JSONResponse(
            status_code=500,
            content={
                "ok": False,
                "detail": "AutoCAD termino pero no se genero el archivo PDF.",
                "log": (proc.stdout or "")[-1500:] + "\n" + (proc.stderr or "")[-500:],
            },
        )

    return {
        "ok": True,
        "job_id": job_id,
        "pdf_url": f"/api/descargar/{job_id}/pdf",
        "dwg_url": f"/api/descargar/{job_id}/dwg",
        "pdf_name": pdf_name,
        "dwg_name": safe_name,
        "log": (proc.stdout or "")[-1200:],
    }


@app.get("/api/descargar/{job_id}/{tipo}")
async def descargar(job_id: str, tipo: str):
    job_dir = JOBS_DIR / job_id
    if not job_dir.is_dir():
        raise HTTPException(status_code=404, detail="Trabajo no encontrado.")

    if tipo == "pdf":
        files = list(job_dir.glob("*_AREAS.pdf"))
        media = "application/pdf"
    elif tipo == "dwg":
        files = list(job_dir.glob("*.dwg"))
        media = "application/acad"
    else:
        raise HTTPException(status_code=400, detail="Tipo de archivo invalido.")

    if not files:
        raise HTTPException(status_code=404, detail="Archivo no disponible.")

    target = files[0]
    return FileResponse(path=str(target), filename=target.name, media_type=media)
