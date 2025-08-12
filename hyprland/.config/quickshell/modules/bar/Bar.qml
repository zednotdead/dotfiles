import Quickshell
import QtQuick
import QtQuick.Layouts
import qs.modules.widgets.workspaces
import qs.modules.widgets.clock
import qs.modules.widgets.tray
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

        color: Theme.barBackground

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

            Workspaces {
                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
            }

            RowLayout {
                layoutDirection: Qt.RightToLeft
                anchors.right: parent.right
                anchors.top: parent.top
                anchors.verticalCenter: parent.verticalCenter

                Clock {}
                SysTray {}
            }
        }
    }

    Corner {
        size: root.cornerShapeSize
        positionX: "left"

        anchors.top: bar.bottom
        anchors.left: bar.left
    }

    Corner {
        size: root.cornerShapeSize
        positionX: "right"

        anchors.top: bar.bottom
        anchors.right: bar.right
    }
}
