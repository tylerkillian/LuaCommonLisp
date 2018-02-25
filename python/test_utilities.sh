assertEquals() {

	testCommand="$1"
	testValue="$($testCommand ; echo -n ".")"
	testValue="${testValue:0:-1}"

	referenceCommand="$2"
	referenceValue="$($referenceCommand ; echo -n ".")"
	referenceValue="${referenceValue:0:-1}"

	if [ "$testValue" != "$referenceValue" ]
	then
		callerInfo=`caller`
		echo "ASSERT FAILED! [Line $callerInfo]"
		echo ""
		echo "EXPECTED:"
		echo "[$referenceValue]"
		echo ""
		echo "GOT:"
		echo "[$testValue]"
		exit 1
	fi
}

