# githapi 0.8.8

## Minor Change

- Added a NEWS.md file


# githapi 0.8.7

## Changes

- Fixed bug introduced into `read_files()` which looked for SHA in the wrong repo


# githapi 0.8.6

## Changes

- Fixed bug in `read_files()` and `download_files()` which prevented them from accessing files 
  in folders


# githapi 0.8.5

## Changes

- Replaced all the logr functions with the msgr package
- Added test coverage reporting
- Added:
  - `files_exist()`
  - `releases_exist()`


# githapi 0.8.2

## Changes

- Refactored the new Git and Repository functions changing the order of the parameters
- Deprecated all the old Git and Repository functions that have been replaced
- added:
  - `shas_exist()`
  - `source_files()`
- removed
  - `view_readme()`


# githapi 0.7.3

## Changes

Added repository commits functions:

- download_commit()
- view_commits()
- view_shas()
- compare_commits()
- compare_files()

Added git blobs functions:

- view_blobs()
- create_blobs()
- update_blobs()
- read_files()
- download_files()
- blob_exists()

Added git trees functions:

- view_trees()
- create_tree()
- upload_tree()
- tree_exists()

Added git commits functions:

- view_commits()
- create_commit()
- upload_commit()
- commit_exists()

Added git branches functions:

- view_branches()
- create_branches()
- update_branches()
- delete_branches()
- branch_exists()

Added git tags functions:

- tag_exists()


# githapi 0.7.2

## Changes

Added:
- view_files()
- create_files()
- update_files()
- delete_files()
- view_readme()
- view_contents()


# githapi 0.7.1

## Changes

No functional changes, change fixed broken tests and refactored new functions.


# githapi 0.7.0

## Changes

This release starts the process of replacing the naming convention of functions and adding 
support for creating, updated and deleting objects in GitHub. It also adds useful messages, 
and logging.

- Added `info()`, `warn()` and `error()` functions which give nice messages to the users and 
  optionally logs them to a file.
- Added `view_tags()`, `create_tags()`, `update_tags()` and `delete_tags()`
- Added `view_releases()`, `create_releases()`, `update_releases()` and `delete_releases()`


# githapi 0.6.3

Resulting from the change to the README in the master branch


# githapi 0.6.2

Added configuration files to get Travis working for continuous integration. Also, fixed a number 
of issues which were causing the build to fail.


# githapi 0.6.1

Converted documentation to markdown and generated a website using `pkgdown`.


# githapi 0.6.0

This release completed replaced the underlying functions to remove dependencies on unnecessary 
packages. As a result, the package loads faster and with fewer conflicts. Tables are generally 
faster to parse also.


# githapi 0.5.2

Fixed a bug where empty columns threw an error. For example requesting the cards, using 
`gh_cards()`, in an empty column of a project.


# githapi 0.5.1

 - gh_commit and gh_tag now return an extra list item called "verification"
 - Update the gh_get test due to change in README


# githapi 0.5.0

 - Added functions for saving and sourcing files from GitHub
 - Added functions to test for existence
 - Added a safe version of select function


# githapi 0.4.0

- added gh_json
- fixed gh_page to return correct number
- added gh_tibble and made gh_page more robust
- added gh_projects
- added gh_project
- added gh_columns
- added gh_cards
- added gh_column
- added gh_card
- gh_page now uses n_max
- removed gh_tibble, gh_json and flatten
- added gh_rate_limit and warning in gh_get
- gh_issues: collapsed assignees login to comma-separated string
- update docs for n_max parameter
- added gh_gists
- added gh_gist
- added gh_gist_commits
- added is_gist_starred
- Changed all functions returning booleans to is_...
- added gh_gist_forks
- added gh_events
- added gh_event
- added gh_commit_comments
- added gh_commit_comment
- added gh_contributers
- extended gh_teams to include repos, without tests
- added gh_languages
- added gh_releases and collapse_list
- added gh_release
- added gh_assets
- added gh_asset
- added gh_gist_comments
- added gh_gist_comment


# githapi 0.3.0

- added gh_label
- added gh_labels
- expanded gh_labels to include issues and milestones
- added gh_milestone
- added gh_milestones
- added gh_organizations
- added gh_organization
- added gh_member
- added gh_members
- added gh_membership and gh_memberships without tests
- added gh_team and gh_teams without tests
- extended gh_members and gh_membership to include teams
- Added the "closed_at" column to gh_issues
- extended gh_repositories to include teams
- added gh_manager without tests
- extended gh_teams to include users teams without tests
- added gh_pull_requests
- added gh_pull_request
- added gh_pull_commits
- added gh_pull_files
- added gh_pull_merged
- added gh_pull_review without tests
- added gh_pull_reviews without tests
- added gh_pull_comments without tests
- added gh_pull_comment without tests
- added gh_pull_review_requests without tests
- added gh_collaborators
- added gh_collaborator
- added gh_permissions
- added gh_user_email


# githapi 0.2.0

Added functions for accessing the Git Data API
