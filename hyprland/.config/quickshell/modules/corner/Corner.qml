import QtQuick
import QtQuick.Shapes

Shape {
    id: root
    required property int size
    required property string positionX
    required property color color

    width: root.size
    height: root.size

    ShapePath {
        fillColor: root.color
        startX: 0
        startY: 0

        PathLine {
            x: root.positionX == "right" ? root.size : 0
            y: 0
        }

        PathLine {
            x: root.positionX == "right" ? root.size : 0
            y: root.size
        }

        PathArc {
            x: root.positionX == "right" ? 0 : root.size
            y: 0
            radiusX: root.size
            radiusY: root.size

            direction: root.positionX == "right" ? PathArc.Counterclockwise : PathArc.Clockwise
        }
    }
}
