//@ pragma Env QS_NO_RELOAD_POPUP=1
//@ pragma Env QT_QUICK_FLICKABLE_WHEEL_DECELERATION=10000
//@ pragma UseQApplication

import Quickshell
import qs.config
import qs.modules.bar

ShellRoot {
    LazyLoader {
        active: Theme.ready
        Bar {}
    }
}
