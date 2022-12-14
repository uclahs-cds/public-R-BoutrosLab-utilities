test_that('latest.project.filename works', {
	output.dir <- tempdir()
	file.create(
		file.path(
			output.dir,
			generate.filename('TestProject', 'myfile', 'txt', file.date = '2022-12-12')
			)
		)
	file.create(
		file.path(
			output.dir,
			generate.filename('TestProject', 'myfile', 'txt', file.date = '2022-12-11')
			)
		)
	file.create(
		file.path(
			output.dir,
			generate.filename('TestProject', 'myfile', 'txt')
			)
		)

	expect_equal(
		basename(latest.project.filename('TestProject', 'myfile', 'txt', folder.path = output.dir)),
		generate.filename('TestProject', 'myfile', 'txt')
		)

	expect_equal(
		basename(
			latest.project.filename('TestProject', 'myfile', 'txt', folder.path = output.dir, file.date = '2022-12-11')
			),
		generate.filename('TestProject', 'myfile', 'txt', file.date = '2022-12-11')
		)
})
