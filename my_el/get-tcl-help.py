#!/usr/bin/python
import os
import sys

def match_file( pathname , targetfile ):
    if os.path.isdir(pathname ):
        for file in os.listdir( pathname ):
            if match_file( pathname+"/"+file , targetfile) == 1:
                return 1
            
    elif os.path.basename(pathname) == targetfile: 
        fid = open( pathname , 'r' )
        print fid.read()
        fid.close()
        return 1
    return 0

help_file_dir_list = [ "/eda/Xilinx/v/doc/eng/" ]


for dir in help_file_dir_list:
    if  match_file( dir , sys.argv[1] ) == 1 :
    #if  match_file( dir , "foreach" ) == 1 :
        break;
else:
    print "can not find the help"


