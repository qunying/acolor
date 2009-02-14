GNAT_FLAGS=-gnatwa -gnata -gnaty3abefiklmnprst

acolor: acolor.adb
	gnatmake $(GNAT_FLAGS) $@

pp:
	gnatpp -rnb acolor.adb

clean:
	@echo "cleaning project directory ...."
	-@rm *.{o,ali} *~ b~*.{adb,ads} >& /dev/null
