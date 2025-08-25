pragma ComponentBehavior: Bound
import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import qs.services.drawer

PopupWindow {
    id: root
    implicitWidth: 500
    implicitHeight: 700
    color: "transparent"

    anchor.gravity: Edges.Top | Edges.Left
    anchor.rect.x: 0

    visible: DrawerState.visible

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 80
            topLeftRadius: 10
            topRightRadius: 10

            gradient: Gradient {
                GradientStop {
                    position: 0.0
                    color: "#3168d5"
                }
                GradientStop {
                    position: 0.09
                    color: "#4993e6"
                }
                GradientStop {
                    position: 0.18
                    color: "#245dd7"
                }
                GradientStop {
                    position: 0.5
                    color: "#245ddb"
                }
            }

            Text {
                anchors.fill: parent
                anchors.leftMargin: 20
                verticalAlignment: Text.AlignVCenter

                text: Quickshell.env("USER") ?? "user"
                style: Text.Raised
                color: "white"
                font.pointSize: 24
                font.bold: true
                font.italic: true
            }
        }

        Rectangle {
            id: box
            color: "white"
            Layout.fillWidth: true
            Layout.fillHeight: true

            ClippingRectangle {
                anchors.fill: parent
                color: "transparent"

                ListView {
                    anchors.fill: parent
                    anchors.topMargin: 10
                    anchors.bottomMargin: 10

                    model: ScriptModel {
                        id: model
                        values: [...DesktopEntries.applications.values]
                    }

                    delegate: appItem
                    ScrollBar.vertical: ScrollBar {}
                }
            }

            Component {
                id: appItem
                AppItem {}
            }
        }
    }
}
