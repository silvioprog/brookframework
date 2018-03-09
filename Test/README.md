# How to do the tests?

Compile the test you want. Now, after generating its executable, open the terminal and run the script related to your system. On Linux:

```
./RunAllTests.sh
```

on Windows:

```cmd
RunAllTests.bat
```

# How to check if the test is OK?

The script will report the result of all tests, for example:

```
$ ./RunAllTests.sh 
Running test ./Test_String OK
Running test ./Test_Utils OK
Running test ./Test_libbrook OK
Total of run tests: 3
```

If you do not get the message `OK` in front the test, then probably it failed.

**Note:** you need to install the Brook library before doing any test, because the most of them depends on it.
