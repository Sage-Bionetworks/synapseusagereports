context("test-lib")

test_acl_json <- '{"id":"syn12509922","resourceAccess":[{"principalId":3323072,"accessType":["DELETE","DOWNLOAD","READ","CHANGE_SETTINGS","UPDATE","MODERATE","CREATE","CHANGE_PERMISSIONS"]}],"etag":"19a13eb7-8d24-4d26-b9d7-b8fa97b8ce92","creationDate":"2018-06-11T18:41:45.244Z","uri":"/entity/syn12509922/acl"}'
test_acl <- jsonlite::fromJSON(test_acl_json, simplifyVector = FALSE)

test_that("acl to user list", {
  expect_equal(aclToMemberList(test_acl), )
})
