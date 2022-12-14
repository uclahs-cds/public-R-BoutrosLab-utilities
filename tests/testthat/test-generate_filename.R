test_that('generate.filename works', {
	validate.regex <- '([0-9]{4}-[0-9]{2}-[0-9]{2}_)?[[:alnum:]-]+_[[:alnum:]-]+\\.[[:alnum:]-]+';
	# Invalid dates
	expect_error(generate.filename('TestProject', 'myfile', 'txt', file.date = '2022-13-12'));
	expect_error(generate.filename('TestProject', 'myfile', 'txt', file.date = '2022-12-32'));
	expect_error(generate.filename('TestProject', 'myfile', 'txt', file.date = 'abc'));

	# Non alphanumeric characters in project or filename
	expect_error(generate.filename('TestProject', 'myfile_abc', 'txt'));
	expect_error(generate.filename('TestProject', 'myfile', 'txt_abc'));
	expect_error(generate.filename('TestProject_abc', 'myfile', 'txt'));
	expect_error(generate.filename('TestProject_abc', 'myfile_abc', 'txt'));
	expect_error(generate.filename('TestProject_abc', 'myfile_abc', 'txt_abc'));

	# Validate against global regular expression
	expect_true(
		grepl(validate.regex, generate.filename('TestProject', 'myfile', 'txt'))
		);
	expect_true(
		grepl(validate.regex, generate.filename('TestProject', 'myfile', 'txt', file.date = NULL))
		);
})
