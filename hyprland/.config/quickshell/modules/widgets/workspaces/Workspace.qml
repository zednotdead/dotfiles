import Quickshell.Widgets
import Quickshell.Hyprland
import QtQuick
import qs.config

WrapperItem {
    id: root
    required property HyprlandWorkspace workspace

    property int size: 20

    Rectangle {
        color: {
            const items = {
                focused: {
                    nohover: Theme.workspaceFocused,
                    hover: Theme.workspaceFocusedHover
                },
                unfocused: {
                    nohover: Theme.workspaceUnfocused,
                    hover: Theme.workspaceUnfocusedHover
                }
            };

            const focusState = root.workspace.active ? "focused" : "unfocused";
            const hoverState = mouse.containsMouse ? "hover" : "nohover";

            return items[focusState][hoverState];
        }

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
