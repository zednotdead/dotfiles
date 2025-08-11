import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Shapes
import qs.modules.widgets.workspaces
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

    property double cornerShapeHeight: 30

    implicitHeight: bgWrapper.childrenRect.height + cornerShapeHeight
    exclusiveZone: bgWrapper.childrenRect.height

    aboveWindows: false

    Item {
        id: bgWrapper

        implicitWidth: childrenRect.width
        implicitHeight: childrenRect.height

        anchors.left: parent.left
        anchors.top: parent.top

        WrapperItem {
            margin: 10
            bottomMargin: 10
            topMargin: 10

            Workspaces {}
        }
    }

    Rectangle {
        id: bg
        z: -1

        color: Theme.color00

        implicitHeight: 40

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
    }

    Shape {
        id: topLeft
        property int size: root.cornerShapeHeight

        width: size
        height: size
        anchors.top: parent.bottom
        anchors.left: parent.left
        anchors.topMargin: -root.cornerShapeHeight

        ShapePath {
            fillColor: bg.color
            startX: 0
            startY: 0

            PathLine {
                x: 0
                y: 0
            }

            PathLine {
                x: 0
                y: topLeft.size
            }

            PathArc {
                x: topLeft.size
                y: 0
                radiusX: topLeft.size
                radiusY: topLeft.size
            }
        }
    }

    Corner {
        color: Theme.color00
        size: root.cornerShapeHeight
        positionX: "right"

        anchors.top: bg.bottom
        anchors.right: bg.right
    }
}
