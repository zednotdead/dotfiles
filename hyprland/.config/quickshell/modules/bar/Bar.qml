import Quickshell
import Quickshell.Widgets
import QtQuick
import qs.modules.widgets.workspaces
import qs.modules.widgets.clock
import qs.config
import qs.modules.corner

PanelWindow {
    id: root
    color: "transparent"

    anchors {
        top: true
        right: true
        left: true
    }

    property double cornerShapeSize: 20
    property int barHeight: 40

    aboveWindows: false
    exclusiveZone: barHeight

    Rectangle {
        id: bar
        z: -1

        color: Theme.color00

        implicitHeight: root.barHeight

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        Item {
            id: bgWrapper

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.top: parent.top
            anchors.leftMargin: 10
            anchors.rightMargin: 10

            anchors.verticalCenter: parent.verticalCenter

            WrapperItem {
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter

                Workspaces {}
            }

            WrapperItem {
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter

                Clock {}
            }
        }
    }

    Corner {
        color: Theme.color00
        size: root.cornerShapeSize
        positionX: "left"

        anchors.top: bar.bottom
        anchors.left: bar.left
    }

    Corner {
        color: Theme.color00
        size: root.cornerShapeSize
        positionX: "right"

        anchors.top: bar.bottom
        anchors.right: bar.right
    }
}
