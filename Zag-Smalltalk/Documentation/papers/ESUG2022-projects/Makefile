PRESENTER=25min
TOPDIR:=~/fs/Courses/Lectures
TARGETS:=Talk.pdf
default:  open #pdfs
pdfs: vision.pdf leadership.pdf
	open $?
%.pdf: %.tex
	make-pdflatex $<
open: 
include $(TOPDIR)/lecture.make
