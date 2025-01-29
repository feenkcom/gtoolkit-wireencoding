#! /bin/bash
### Install gtoolkit-wireencoding from Tonel files into a Rowan-enabled stone
### Exits with 0 if success, 1 if failed

gt4GemstoneHome=${ROWAN_PROJECTS_HOME}/gtoolkit-wireencoding
## Topaz refuses to exit from script if input is stdin, so redirect from /dev/zero
topaz -l -I ${gt4GemstoneHome}/scripts/loginSystemUser.topaz  -S ${gt4GemstoneHome}/scripts/installGToolkit-wireencoding.topaz < /dev/null
if [ $? = 0 ]
    then
        echo gtoolkit-wireencoding loaded
    else
        echo !!!!!!!!!!!!!!
        echo INSTALL FAILED for gtoolkit-wireencoding
        echo !!!!!!!!!!!!!!
        exit 1
    fi

