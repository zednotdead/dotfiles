pragma ComponentBehavior: Bound
import Quickshell.Services.Pipewire
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import qs.config
import qs.modules.widgets

RowLayout {
    id: root
    required property PwNode node
    Layout.fillWidth: true
    spacing: 10

    IconImage {
        id: icon
        Layout.horizontalStretchFactor: 1
        visible: source != ""
        source: {
            const icon = root.node.properties["application.icon-name"] ?? "audio-volume-high-symbolic";
            return `image://icon/${icon}`;
        }

        implicitSize: 22
    }

    ColumnLayout {
        Layout.fillWidth: true
        Layout.horizontalStretchFactor: 9

        RowLayout {
            Layout.fillWidth: true
            StyledText {
                Layout.fillWidth: true

                elide: Label.ElideRight
                text: {
                    // application.name -> description -> name
                    const app = root.node.properties["application.name"] ?? (root.node.description != "" ? root.node.description : root.node.name);
                    const media = root.node.properties["media.name"];
                    return media != undefined ? `${app} - ${media}` : app;
                }
            }
        }

        RowLayout {
            Layout.fillWidth: true

            StyledText {
                text: `${Math.floor(root.node.audio.volume * 100)}%`
            }

            Slider {
                Layout.fillWidth: true
                value: root.node.audio.volume

                onValueChanged: function () {
                    root.node.audio.muted = false;
                    root.node.audio.volume = value;
                }
            }
        }
    }

    Button {
        id: muteButton
        icon.name: root.node.audio.muted ? "player-volume-muted" : "player-volume"
        onClicked: root.node.audio.muted = !root.node.audio.muted
        Layout.horizontalStretchFactor: 1
        background: Rectangle {
            anchors.fill: parent
            implicitWidth: 20
            implicitHeight: 20
            radius: 20
            color: Theme.color01
        }
    }

    PwObjectTracker {
        objects: [root.node]
    }
}
