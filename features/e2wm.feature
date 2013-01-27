Feature: Simple window management
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


Feature: History management
  In order to organize buffers
  As a user
  I want to manage history of opened buffers

  Scenario: Open three recordable buffers
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to a buffer "recordable-buffer-a"
     And I switch to a buffer "recordable-buffer-b"
     And I switch to a buffer "recordable-buffer-c"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-c |
      |         | recordable-buffer-b |
      |         | recordable-buffer-a |
