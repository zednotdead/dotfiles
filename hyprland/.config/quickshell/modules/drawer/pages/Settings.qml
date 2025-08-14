pragma ComponentBehavior: Bound
import qs.config
import Quickshell.Widgets
import Quickshell.Services.Pipewire
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import qs.modules.widgets.mixer

BasePage {
    id: root
    title: "Mixer"

    RowLayout {
        Item {
            Layout.fillHeight: true
            Layout.fillWidth: true
            Layout.horizontalStretchFactor: 1
            Layout.preferredWidth: root.width * (1 / 3)

            ClippingRectangle {
                anchors.fill: parent
                topLeftRadius: 10
                bottomLeftRadius: 10

                Image {
                    anchors.fill: parent
                    fillMode: Image.PreserveAspectCrop
                    source: Qt.resolvedUrl(Theme.wallpaper)
                }
            }
        }
        Item {
            id: rightBox
            Layout.fillHeight: true
            Layout.fillWidth: true
            Layout.horizontalStretchFactor: 2
            Layout.preferredWidth: root.width * (2 / 3)

            Rectangle {
                anchors.fill: parent
                topRightRadius: 10
                bottomRightRadius: 10
                color: Qt.lighter(root.color, 1.8)

                ScrollView {
                    id: scroll
                    anchors.fill: parent

                    property int margin: 10
                    anchors.leftMargin: margin
                    anchors.rightMargin: margin
                    anchors.topMargin: margin

                    ColumnLayout {
                        anchors.fill: parent
                        Repeater {
                            model: {
                                return Pipewire.nodes.values.filter(o => o.isStream);
                            }

                            Mixer {
                                required property PwNode modelData
                                node: modelData
                                Layout.fillWidth: true
                            }
                        }
                    }
                }
            }
        }
    }
}
