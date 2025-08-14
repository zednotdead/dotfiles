pragma ComponentBehavior: Bound
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import qs.config
import qs.modules.widgets

Item {
    id: root
    anchors.fill: parent
    required property string title
    property color color: Qt.lighter(Theme.background, 1.8)

    ColumnLayout {
        anchors.fill: parent
        StyledText {
            text: root.title
            font.pointSize: 18
            Layout.fillWidth: true
        }

        Rectangle {
            id: bg
            color: root.color
            z: -1

            Layout.fillWidth: true
            Layout.fillHeight: true

            topRightRadius: 20
            topLeftRadius: 20

            WrapperItem {
                id: slot
                anchors.fill: parent
                anchors.margins: 20

                children: root.children
            }
        }
    }
}
