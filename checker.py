import argparse
import sys
import os
from subprocess import check_output, CalledProcessError
from test_scripts import command_skel, tests_dict, points_dict

"""
Tests name convention: <predicate>_<table_name>_<test_no>
"""

movies_table = ''
ratings_table = ''
students_table = ''

def generate_ref_files(hwfile):
    path = os.getcwd()
    for root, dirs, files in os.walk(path):
        dirnames = dirs
        break

    for test_set in tests_dict:
        if test_set.find('tprint') != -1:
            test_set_name = 'tprint'
        elif test_set.find('select') != -1:
            test_set_name = 'select'
        elif test_set.find('join') != -1:
            test_set_name = 'join'
        elif test_set.find('tfilter') != -1:
            test_set_name = 'tfilter'
    
        for test_name in tests_dict[test_set]:
            reffile = 'ref/' + test_set_name + '/' + test_name + '.ref'
            run_test(hwfile, test_name, reffile, True)

def run_all(generateref, hwfile):
    print('Prolog Homework 2 checker\n')

    path = os.getcwd()
    for root, dirs, files in os.walk(path):
        dirnames = dirs
        break
    if not ('out' in dirnames):
        path += '/out/'
        try:
            os.mkdir(path)
        except OSError:
            print ("Creation of the directory %s failed" % path)
            return

    total = 0
    for test_set in tests_dict:
        if test_set.find('tprint') != -1:
            test_set_name = 'tprint'
        elif test_set.find('select') != -1:
            test_set_name = 'select'
        elif test_set.find('join') != -1:
            test_set_name = 'join'
        elif test_set.find('tfilter') != -1:
            test_set_name = 'tfilter'
        elif test_set.find('complex_query1') != -1:
            test_set_name = 'complex_query1'
        else:
            test_set_name = 'complex_query2'
        
        if not generateref:
            total += run_test_set(test_set_name, hwfile)
            """
            print('Testing ' + test_set_name + '...')
            for test_name in tests_dict[test_set]:
                reffile = 'ref/' + test_set_name + '/' + test_name + '.ref'
                points = run_test(hwfile, test_name, reffile, False)
                total += points
            print()
            """
    
    if not generateref:
        print('Total' + '...............................[' + str(total) + 'p]')

def run_test_set(test_set_name, hwfile):
    test_set = ''

    if test_set_name == 'tprint':
        test_set = 'eval_tprint'
    elif test_set_name == 'select':
        test_set = 'eval_select'
    elif test_set_name == 'join':
        test_set = 'eval_join'
    elif test_set_name == 'tfilter':
        test_set = 'eval_tfilter'
    elif test_set_name == 'complex_query1':
        test_set = 'eval_complex_query1'
    elif test_set_name == 'complex_query2':
        test_set = 'eval_complex_query2'

    if test_set is '':
        print('Invalid test set name')
        return 0

    print('Testing ' + test_set_name + '...')
    test_set_total = 0
    for test_name in tests_dict[test_set]:
        reffile = 'ref/' + test_set_name + '/' + test_name + '.ref'
        points = run_test(hwfile, test_name, reffile, False)
        test_set_total += points
    print("\nTotal for " + test_set_name + '...............................[' + str(test_set_total) + 'p]')
    print()
    
    return test_set_total

def run_test(hwfile, testname, reffile, generateref):
    
    # determine test script
    test_dict = None
    if testname.find('tprint') != -1:
        test_dict = tests_dict['eval_tprint']
    elif testname.find('select') != -1:
        test_dict = tests_dict['eval_select']
    elif testname.find('join') != -1:
        test_dict = tests_dict['eval_join']
    elif testname.find('filter') != -1:
        test_dict = tests_dict['eval_tfilter']
    elif testname.find('complex_query1') != -1:
        test_dict = tests_dict['eval_complex_query1']
    elif testname.find('complex_query2') != -1:
        test_dict = tests_dict['eval_complex_query2']

    try :    
        test_script = test_dict[testname]
    except KeyError:
        print('Invalid testname')
        return 0
    command = command_skel.format(hwfile, test_script)
    print(command)

    # run test, check for errors and write output file
    out = ''
    try:
        out = check_output(command, shell=True)
        out = out.decode("utf-8")
    except CalledProcessError as grepexec:
        if grepexec.returncode is 1:
            print(testname + '...............................' + 'false')
            return 0
        else:
            print(testname + '...............................' + 'error')
            return 0

    # in case generateref is active
    if generateref:
        with open(reffile, "w+") as reffd:
            reffd.write(out)
        return 0

    # check results
    outfile = 'out/' + testname + '.out'
    with open(outfile, "w+") as outfd:
        outfd.write(out)
    
    command = 'diff ' + outfile + ' ' + reffile
    try:
        ret = check_output(command, shell=True)
        ret = ret.decode("utf-8")
    except CalledProcessError as grepexec:
        print(testname + '...............................' + '[0p]')
        return 0

    points = points_dict[testname]
    print(testname + '...............................' + '[' + str(points) + 'p]')
    return points

if __name__ == '__main__':
    """
    Command format:
        python3 check_test.py [--swiplexe <executable's name>] [--hwfile <file_name>.pl]
                                [--testname <test_name>] [--reffile <reffile_path>]
                                [--testsetname <test_set_name>]
    Default: all tests are run
    """
    parser = argparse.ArgumentParser(description='Prolog Assignment Checker')
    parser.add_argument('--hwfile', default='main.pl',
                        help='Path to the homework file; default: main.pl')
    parser.add_argument('--testname',
                        help='The name of the test to run; example: select_movies_1')
    parser.add_argument('--reffile',
                        help='Path to the reference file; example: ref/out0.ref')
    parser.add_argument('--testsetname',
                        help='Name of the test set; options: tprint, select, join, \
                              tfilter, complex_query1, complex_query2')
    parser.add_argument('--generateref', action="store_true",
                        help='Internal use only. Do not activate. WILL OVERWRITE REFERENCE FILES.')
    args = parser.parse_args()

    if (args.testname and not args.reffile) or (not args.testname and args.reffile):
        sys.exit('If you don\'t want to run all tests, you must specify both --testfile and --reffile parameters')

    if args.testname and args.reffile:
        run_test(args.hwfile, args.testname, args.reffile, False)
    elif args.generateref:
        generate_ref_files(args.hwfile)
    elif args.testsetname:
        run_test_set(args.testsetname, args.hwfile)
    else:
        run_all(False, args.hwfile)
