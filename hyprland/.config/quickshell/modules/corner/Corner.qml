import QtQuick
import QtQuick.Shapes
import qs.config

Shape {
    id: root
    required property int size
    required property string positionX

    width: root.size
    height: root.size

    ShapePath {
        fillColor: Theme.barBackground
        strokeWidth: 0
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
