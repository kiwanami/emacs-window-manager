Feature: Simple window management
  In order to have a happy Emacs life
  As a user
  I want to manage Emacs windows

  Scenario: Toggle imenu window
    Given I enabled e2wm
    When I switch to "code" perspective
    Then I should see window "imenu"
    When I press "C-c ; I"
    Then I should not see window "imenu"

  Scenario: Pop to help buffer
    Given I enabled e2wm
    When I switch to "code" perspective
    Then I should be in window "main"
    When I have a popup buffer "*Help*"
    Then I should be in window "sub"
     And I should be in buffer "*Help*"

  Scenario: Toggle maximize
    Given I enabled e2wm
    When I switch to "code" perspective
    Then I should see these windows:
      | Window names |
      | main         |
      | files        |
      | history      |
      | imenu        |
     And I press "C-c ; M"
    Then I should see these windows:
      | Window names |
      | main         |
     And I press "C-c ; M"
    Then I should see these windows:
      | Window names |
      | main         |
      | files        |
      | history      |
      | imenu        |

  Scenario: When in left window, open buffer in right window
    Given I enabled e2wm
    When I switch to "stwo" perspective
    Then I should be in window "left"
     And I switch to a buffer "recordable-1"
    When I have a popup buffer "recordable-2"
    Then I should be in window "right"
     And I should be in buffer "recordable-2"
    When I switch to window "left"
    Then I should be in buffer "recordable-1"

  Scenario: When in right window, open buffer in left window
    Given I enabled e2wm
    When I switch to "stwo" perspective
     And I switch to window "right"
     And I switch to a buffer "recordable-1"
    When I have a popup buffer "recordable-2"
    Then I should be in window "left"
     And I should be in buffer "recordable-2"
    When I switch to window "right"
    Then I should be in buffer "recordable-1"

  Scenario: Do not show blank buffer when opening a file in VC (#46)
    Given I enabled e2wm
    When I switch to "stwo" perspective
    Then I should be in window "left"
     And I switch to a buffer "recordable-1"
     And I execute a command that reopens buffer "recordable-2" in other window
    Then I should be in window "right"
     And I should be in buffer "recordable-2"
    When I switch to window "left"
     And I should be in buffer "recordable-1"

  Scenario: Killing the blank buffer should not cause a problem (#42)
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to a buffer " *e2wm:blank*"
     And I press "C-x k RET"
     And I switch to buffer "recordable"
     And I press "C-x k RET"

  @failing
  Scenario: Completing window should not move focused window
    Given I enabled e2wm
    When I switch to "stwo" perspective
     And I switch to window "right"
     And I press "C-x C-f ~ / TAB TAB"
    Then I should see buffer "*Completions*" in window "sub"
     And I quit
    Then I should not see window "sub"
    Then I should be in window "right"
