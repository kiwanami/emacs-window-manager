Feature: Perspective "two"
  In order to work with two buffers side-by-side
  As a user
  I want to use built-in "two" perspective

  Scenario: Swap buffer when in left window
    Given I enabled e2wm
     When I switch to "two" perspective
      And I switch to window "left" and open buffer "recordable-left"
      And I switch to window "right" and open buffer "recordable-right"
      And I switch to window "left"
     Then I should be in buffer "recordable-left"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-right"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-left"

  Scenario: Swap buffer when in right window
    Given I enabled e2wm
     When I switch to "two" perspective
      And I switch to window "left" and open buffer "recordable-left"
      And I switch to window "right" and open buffer "recordable-right"
     Then I should be in buffer "recordable-right"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-left"
     When I press "C-c ; -"
     Then I should be in buffer "recordable-right"
