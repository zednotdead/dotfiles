import Quickshell.Widgets
import Quickshell.Hyprland
import QtQuick
import qs.config

WrapperItem {
    id: root
    required property HyprlandWorkspace workspace

    property int size: 20

    Rectangle {
        color: root.workspace.active ? Theme.color10 : Theme.color01
        implicitHeight: root.size
        implicitWidth: 20
        radius: 20

        MouseArea {
            anchors.fill: parent
            onClicked: Hyprland.dispatch("workspace " + root.workspace.id)
        }
    }
}
