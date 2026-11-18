# Portal Web de Áreas y PDF para AutoCAD

Este paquete permite que tu equipo suba archivos `.dwg` desde Google Chrome y obtenga automáticamente el `.dwg` procesado y el `.pdf` en formato ISO expand A1 horizontal.

## Requisitos en la PC servidor
1. Windows con **AutoCAD** instalado (incluye `accoreconsole.exe`).
2. **Python 3.10+** instalado.

## Instalación y arranque
1. Descomprime esta carpeta en la PC que tiene AutoCAD (por ejemplo `C:\PortalAreas`).
2. Abre una terminal en esa carpeta e instala dependencias:
   ```cmd
   pip install -r requirements.txt
   ```
3. Inicia el servidor web:
   ```cmd
   uvicorn server:app --host 0.0.0.0 --port 8000
   ```
   *(Si `accoreconsole.exe` está en una ruta personalizada, define antes: `set AUTOCAD_CORE_PATH=C:\Ruta\accoreconsole.exe`)*
4. Abre el puerto `8000` en el Firewall de Windows y comparte con tu equipo la URL local en Google Chrome:
   `http://<IP-DE-ESTA-PC>:8000`
