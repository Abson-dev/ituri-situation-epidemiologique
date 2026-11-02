@echo off
setlocal
title Tableau de bord - Surveillance Ituri
cd /d "%~dp0"

set "RSCRIPT="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT for /d %%d in ("%LOCALAPPDATA%\Programs\R\R-*" "%ProgramFiles%\R\R-*") do if exist "%%~d\bin\Rscript.exe" set "RSCRIPT=%%~d\bin\Rscript.exe"

if not defined RSCRIPT (
  echo.
  echo R est introuvable. Installez-le depuis https://cran.r-project.org puis relancez ce fichier.
  echo.
  pause
  exit /b 1
)

echo.
echo   Tableau de bord - Surveillance Ituri
echo   ------------------------------------
echo   Demarrage en cours : le navigateur va s'ouvrir.
echo   Gardez cette fenetre ouverte. Fermez-la pour arreter le tableau de bord.
echo.

"%RSCRIPT%" lancer.R %*
if errorlevel 1 (
  echo.
  echo Le tableau de bord s'est arrete sur une erreur. Le message se trouve ci-dessus.
  echo.
  pause
)
endlocal
