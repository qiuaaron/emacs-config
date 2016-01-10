
import os

xilinx_man_dir = r"/eda/Xilinx/v/doc/eng/"
xilinx_man_subdir = [ r"class/",  r"man/",  r"tcl/" ]

for dir in xilinx_man_subdir:
    if ( os.path.exists(dir) and os.path.isdir(dir) ) :
        pass
    else:
        os.makedirs(dir)

for dir in xilinx_man_subdir :
    for file in  os.listdir(xilinx_man_dir+dir):
        print "the file is "+ file+"\n"
        fidr = open( xilinx_man_dir+dir+file ,"r" );
        fidw = open( dir+os.path.basename( file ) ,'w')
        #write the comment
        fidw.writelines("# the snippet is " + os.path.basename( file ) +"\n");
        fidw.writelines("# --"+"\n")
        fidw.write(file)
        fidw.close()
        fidr.close()
        


