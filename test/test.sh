#!/bin/bash

# Update the repository, and use the fresh repository to run the tests.
report_bug_to="megan@amnh.org"
temp="tmp_mail"
echo "Updating from the repository"
cd ../
if svn update &> svn.log
then
    cd ./test
    bash ./run_test.sh
else
    echo "I could not update the repository"
    echo "Reporting the error to ${report_bug_to}"
    cat > ${temp} <<EOF
From: $USER@$HOSTNAME
To: ${report_bug_to}
Subject: Test Failure

I could not update from the repository to test the latest version of POY. The
error message is:
`cat svn.log`
EOF
fi
