import Quickshell
import qs.services.drawer

PopupWindow {
    implicitWidth: 500
    implicitHeight: 500

    anchor.gravity: Edges.Top | Edges.Left
    anchor.rect.x: 0

    visible: DrawerState.visible
}
