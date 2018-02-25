assertEquals() {
	testCommand="$1"
	referenceValue="$2"
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

