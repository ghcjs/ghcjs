#######################################################################
# automatic installation/testing of GHCJS on Windows using MinGHC
# run in PowerShell as Administrator:
#
#     Set-ExecutionPolicy Unrestricted
#     Invoke-WebRequest https://raw.githubusercontent.com/ghcjs/ghcjs/master/utils/install.ps1 -OutFile install.ps1
#     .\install.ps1
#
#######################################################################

#######################################################################
# prepare the GHCJS source distribution archive on a POSIX system by running 'cabal sdist'
# this step requires autoreconf, patch, git etc.

$ghcjs  = "http://ghcjs.luite.com/master.tar.gz"

$minghc = "https://github.com/fpco/minghc/releases/download/2015-08-13/minghc-7.10.2-i386.exe"
$node   = "http://nodejs.org/dist/v0.12.7/node-v0.12.7-x86.msi"

#######################################################################

New-Item -ItemType directory -Path C:\haskell
New-Item -ItemType directory -Path C:\haskell\download
New-Item -ItemType directory -Path C:\haskell\tmp
New-Item -ItemType directory -Path C:\haskell\ghcjs
Set-Location -Path C:\haskell\download
Invoke-WebRequest $minghc -OutFile minghc.exe
Invoke-WebRequest $node   -OutFile node.msi
Invoke-WebRequest $ghcjs  -OutFile ghcjs.tar.gz

Start-Process minghc.exe -Argumentlist /S, /D=C:\haskell\ghc -Wait
Start-Process msiexec -Argumentlist /i, node.msi, /qb, INSTALLDIR="C:\haskell\node" -Wait

cmd /c "C:\haskell\ghc\switch\minghc.bat && set > C:\haskell\tmp\environment.txt"
Get-Content C:\haskell\tmp\environment.txt | Foreach-Object {
    if($_ -match "^(.*?)=(.*)$")
    {
        Set-Content "env:\$($matches[1])" $matches[2]
    }
}
$env:Path += ";C:\haskell\node"

Start-Process cabal update -Wait -NoNewWindow

# build and boot GHCJS
Set-Location -Path C:\haskell\ghcjs
Start-Process cabal -Argumentlist unpack, ..\download\ghcjs.tar.gz -Wait -NoNewWindow
Set-Location -Path C:\haskell\ghcjs\ghcjs-*
Start-Process cabal -Argumentlist install, --enable-tests, --enable-benchmarks -Wait -NoNewWindow
Start-Process ghcjs-boot -Wait -NoNewWindow
Set-Location -Path .\test
Start-Process cabal -Argumentlist install -Wait -NoNewWindow

# run the test suite
Start-Process ghcjs-testsuite -Wait -NoNewWindow -RedirectStandardOutput C:\haskell\ghcjs-testsuite.log


