Feature: Multiple frame support
  In order to manage windows in multiple frames
  As a user
  I want to use E2WM with multiple frames

  Scenario: Comeback from unmanaged frame
    Given I enabled e2wm
     Then I should be in perspective "code"
     When I press "C-x 5 2"
     Then I should not be in e2wm-managed frame
     When I press "C-x 5 o"
     Then I should be in e2wm-managed frame
      And I should be in perspective "code"
