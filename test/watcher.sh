while $(true); do
	inotifywait -emodify -r ../src/ pass_files/ fail_files/;
	./run_tests.sh;
done
