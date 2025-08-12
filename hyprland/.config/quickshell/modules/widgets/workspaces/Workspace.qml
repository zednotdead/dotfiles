import Quickshell.Widgets
import Quickshell.Hyprland
import QtQuick
import qs.config

WrapperItem {
    id: root
    required property HyprlandWorkspace workspace

    property int size: 20

    Rectangle {
        color: root.workspace.active ? mouse.containsMouse ? Theme.workspaceFocusedHover : Theme.workspaceFocused : mouse.containsMouse ? Theme.workspaceUnfocusedHover : Theme.workspaceUnfocused

        implicitHeight: root.size
        implicitWidth: root.size
        radius: root.size

        MouseArea {
            id: mouse
            anchors.fill: parent
            hoverEnabled: true
            onClicked: Hyprland.dispatch("workspace " + root.workspace.id)
        }
    }
}
