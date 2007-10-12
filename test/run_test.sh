#!/bin/bash

# This script compiles POY, copies the config.test as the new config, runs the
# battery of tests and emails us if there is some error.

test_program="poy_test"
list_of_tests=$3
test_execution_script="ocaml unix.cma test_line.ml"
report_bug_to="avaron@amnh.org"
temp="tmp_mail"

# We start by checking that everything is good, we need to ensure that we have a
# reasonable configuration, and that we have a completely up to date setup.
cd ../
if ./configure --enable-interface=readline $1;
then 
    echo "Finished configuring the system."
else
    echo "I could not find the configuration file ${config_location}.test"
    echo "Aborting the test run in `hostname`"
    cat > ${temp} <<EOF
From: $USER@$HOSTNAME
To: ${report_bug_to}
Subject: Test Failure

The test $2 in `hostname` falied because I couldn't find a suitable config.test
to compile and run the tests.
EOF
    sendmail ${report_bug_to} < ${temp}
    exit 1
fi

# Now we can make the test program and proceed to run the test suite in this
# computer 
echo "Making poy_test"
cd ./src
if make clean &> ../test/make.log && make depend &>../test/make.log && make poy_test &> ../test/make.log
then
    echo "Finished making poy_test"
else
    echo "There was an error while attempting to build the poy_test in `hostname`."
    echo "The following is the log from the make attempt:"
    echo "Reporting the error to ${report_bug_to}"
    cat > ${temp} <<EOF
From:$USER@$HOSTNAME
To: ${report_bug_to}
Subject: Test Failure

The test $2 in `hostname` failed to make the ${test_program} executable. The log of
the attempt to make is:
`cat ../test/make.log`
EOF
    sendmail ${report_bug_to} < ${temp}
    cd ../test
    exit 1
fi
mv ./poy_test ../test/
cd ../test/
rm -f test_all.log

# Now we run all the tests we have on list
echo "Running tests in `hostname`"
if cat ${list_of_tests} | xargs -L 1 ${test_execution_script} >> test_all.log
then
    grep FAILED test_all.log > test.log
else
    grep FAILED test_all.log > test.log
    echo "There appears to be a crashed process."
    echo "I am reporting it to ${report_bug_to}"
    cat > ${temp} <<EOF
From: $USER@$HOSTNAME
To: ${report_bug_to}
Subject: Test Failure

The test execution in `hostname` appear to have had a crash. The log of the
failed processes is:
`cat test_all.log`
EOF
fi

echo "Finished tests in `hostname`"

if [ -s "test.log" ]
then
    echo "There appear to be some errors in the test.log file."
    echo "Reporting the error to ${report_bug_to}"
    cat > ${temp} <<EOF
From: $USER@$HOSTNAME
To: ${report_bug_to}
Subject: Test Failure

The test $2 execution in `hostname` failed to pass all the unit tests. The log of
the failures is:
`cat ./test.log`
EOF
    sendmail ${report_bug_to} < ${temp}
    cd ../test
    exit 1
else
    cd ../test
    exit 0
fi
