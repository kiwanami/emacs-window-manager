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

  Scenario: Frame local keymap
    Given I enabled e2wm
     Then "e2wm:stop-management" should be called when I type "C-c ; Q"
     When I press "C-x 5 2"
     Then I should not be in e2wm-managed frame
      And key-binding "C-c ;" is undefined
     When I press "C-x 5 o"
     Then I should be in e2wm-managed frame
      And "e2wm:stop-management" should be called when I type "C-c ; Q"

  Scenario: Frame/perspective local keymap
    Given I enabled e2wm
     When I switch to "two" perspective
      And I switch to window "left" and open buffer "recordable-left"
      And I switch to window "right" and open buffer "recordable-right"
     When I press "C-x 5 2"
     Then I should not be in e2wm-managed frame
     When I press "C-x 5 o"
     Then I should be in e2wm-managed frame
      And I should be in perspective "two"
      And I should be in buffer "recordable-right"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-left"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-right"

  Scenario: Manage windows in two frames
    Given I enabled e2wm
     When I press "C-x 5 2"
     Then I should not be in e2wm-managed frame
    Given I enabled e2wm
     Then I should be in e2wm-managed frame
     When I press "C-x 5 o"
     Then I should be in e2wm-managed frame

  Scenario: Stopping window management of already dead frame
    Given I enabled e2wm
     When I press "C-x 5 2"
      And I press "C-x 5 1"
      And I disabled e2wm
     Then I should not be in e2wm-managed frame

  Scenario: Different perspective on different frame
    Given I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
     Then I should be in perspective "code"
     When I press "C-x 5 o"
      And I switch to "two" perspective
      And I press "C-x 5 o"
     Then I should be in perspective "code"
      And "e2wm:dp-code-imenu-toggle-command" should be called when I type "C-c ; I"
      And I press "C-x 5 o"
     Then I should be in perspective "two"
      And "e2wm:dp-two-swap-buffers-command" should be called when I type "C-c ; -"

  Scenario: Stop one of managed frame
    Given I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
     When I disabled e2wm
     Then I should not be in e2wm-managed frame
     When I press "C-x 5 o"
     Then I should be in e2wm-managed frame
      And "e2wm:dp-code-imenu-toggle-command" should be called when I type "C-c ; I"

  Scenario: Stop all managed frames
    Given I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
     When I disabled e2wm
      And I press "C-x 5 o"
      And I disabled e2wm
     Then I should not be in e2wm-managed frame
      And key-binding "C-c ;" is undefined
      And I press "C-x 5 o"
      And I should not be in e2wm-managed frame
      And key-binding "C-c ;" is undefined

  Scenario: Restart one of managed frame
    Given I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
      And I press "C-x 5 o"
      And I disabled e2wm
      And I press "C-x 5 o"
      And I disabled e2wm
      And I press "C-x 5 o"
      And I enabled e2wm
     Then I should be in e2wm-managed frame
      And "e2wm:dp-code-imenu-toggle-command" should be called when I type "C-c ; I"
     When I press "C-x 5 o"
     Then I should not be in e2wm-managed frame
      And key-binding "C-c ;" is undefined

  Scenario: Original display-buffer-function should be used in unmanaged frame
    Given I have custom display-buffer-function
      And I enabled e2wm
     Then my custom display-buffer-function should not be enabled
     When I press "C-x 5 2"
     Then my custom display-buffer-function should be enabled
     When I press "C-x 5 o"
     Then my custom display-buffer-function should not be enabled

  Scenario: Forcefully disable multiple frames
    Given I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
      And I press "C-x 5 2"
      And I enabled e2wm
     Then "e2wm:pst-minor-mode" is on
     When I disabled e2wm forcefully
     Then "e2wm:pst-minor-mode" is off
     Then I should not be in e2wm-managed frame
     When I press "C-x 5 0"
     Then I should not be in e2wm-managed frame
     When I press "C-x 5 0"
     Then I should not be in e2wm-managed frame
