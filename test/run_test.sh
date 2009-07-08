#!/bin/bash

# This script compiles POY, copies the config.test as the new config, runs the
# battery of tests and emails us if there is some error.

# System dependent options
case `../gnu/config.guess` in
    *-cygwin*)
        test_program="poy_test.exe"
        ;;
    *)
        test_program="poy_test"
        ;;
esac
list_of_tests=$3
number_of_processors=$4
test_execution_script="ocaml unix.cma str.cma test_line.ml"

report_bug_to="megan@amnh.org"

temp="tmp_mail"

# We start by checking that everything is good, we need to ensure that we have a
# reasonable configuration, and that we have a completely up to date setup.
cd ../
if ./configure --enable-interface=flat $1;
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
    sendmail -t < ${temp}
    exit 1
fi

# Now we can make the test program and proceed to run the test suite in this
# computer 
echo "Making ${test_program}"
cd ./src
if make clean &> ../test/make.log && make depend &>../test/make.log && ocamlbuild poy_test.native &> ../test/make.log && cp poy_test.native poy_test
then
    echo "Finished making ${test_program}"
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
    sendmail -t < ${temp}
    cd ../test
    exit 1
fi
mv ./${test_program} ../test/
cd ../test/
# We cleanup all of the test log files.
rm -f test_al*.log
rm -f test*.xml

# Now we run all the tests we have on list
echo "Running tests in `hostname`"
if ocaml unix.cma concurrent_test.ml -scriptsfile ${list_of_tests} -p $4
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
    sendmail -t < ${temp}
    cd ../test
    exit 1
else
    cd ../test
    exit 0
fi
