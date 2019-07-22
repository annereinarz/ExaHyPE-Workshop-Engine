folders=(pdt src visualisation)
files=(README.md)a

export DATE=`date +%Y-%m-%d`

peanoVersion="Peano-3.0-$DATE-$(git log --format="%h" -n 1 .)"

tarName="$peanoVersion.tar.gz"

echo $tarName

tar --exclude-vcs --exclude=*.o -czvf  $tarName ${folders[*]} ${files[*]}

