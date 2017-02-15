Feature: History management
  In order to organize buffers
  As a user
  I want to manage history of opened buffers

  Scenario: Open three recordable buffers
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to buffer "recordable-buffer-a"
     And I switch to buffer "recordable-buffer-b"
     And I switch to buffer "recordable-buffer-c"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-c |
      |         | recordable-buffer-b |
      |         | recordable-buffer-a |

  Scenario: Ignore unrecordable buffers
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to buffer "unrecordable-buffer-1"
     And I switch to buffer "recordable-buffer-a"
     And I switch to buffer "unrecordable-buffer-2"
     And I switch to buffer "recordable-buffer-b"
     And I switch to buffer "unrecordable-buffer-3"
     And I switch to buffer "recordable-buffer-c"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-c |
      |         | recordable-buffer-b |
      |         | recordable-buffer-a |

  Scenario: Go back and forth
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to buffer "recordable-buffer-a"
     And I switch to buffer "recordable-buffer-b"
     And I switch to buffer "recordable-buffer-c"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-c |
      |         | recordable-buffer-b |
      |         | recordable-buffer-a |
     And I go back history
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      |         | recordable-buffer-c |
      | ->      | recordable-buffer-b |
      |         | recordable-buffer-a |
     And I go back history
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      |         | recordable-buffer-c |
      |         | recordable-buffer-b |
      | ->      | recordable-buffer-a |
     And I go forward history
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      |         | recordable-buffer-c |
      | ->      | recordable-buffer-b |
      |         | recordable-buffer-a |

  Scenario: Open and then kill three recordable buffers
    Given I enabled e2wm
    When I switch to "code" perspective
     And I switch to buffer "recordable-buffer-a"
     And I switch to buffer "recordable-buffer-b"
     And I switch to buffer "recordable-buffer-c"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-c |
      |         | recordable-buffer-b |
      |         | recordable-buffer-a |
     And I press "C-x k RET"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-b |
      |         | recordable-buffer-a |
     And I press "C-x k RET"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
      | ->      | recordable-buffer-a |
     And I press "C-x k RET"
    Then I should have these buffers in history:
      | Pointer | Buffer Name         |
