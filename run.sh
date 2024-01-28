termExpl

oldDir=$(pwd)

newDir=$(bat /tmp/haskell_output)
echo "Changing to '$newDir'"

cd $newDir
echo "Done changing to '$newDir'\nWas in: '$oldDir'"
