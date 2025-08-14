import QtQuick
import QtQuick.Layouts
import Quickshell.Widgets
import qs.services.notifications
import qs.modules.widgets
import qs.config

WrapperRectangle {
    id: root
    color: Theme.background

    required property Notifs.Notif notification
    Layout.fillWidth: true
    margin: 20
    radius: 20

    RowLayout {
        spacing: 20
        ClippingWrapperRectangle {
            visible: !!root.notification.appIcon
            radius: 10
            Layout.horizontalStretchFactor: 1
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.maximumHeight: 100
            Layout.maximumWidth: root.width * 0.2
            Layout.alignment: Qt.AlignRight

            Image {
                fillMode: Image.PreserveAspectCrop
                source: Qt.resolvedUrl(root.notification.appIcon)
            }
        }

        ColumnLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignTop | Qt.AlignLeft
            Layout.horizontalStretchFactor: 8

            StyledText {
                text: root.notification.appName
                font.pointSize: 16
            }
            StyledText {
                text: root.notification.summary
                wrapMode: Text.Wrap
                horizontalAlignment: Text.AlignLeft
                Layout.fillWidth: true
            }
        }

        StyledButton {
            text: "Dismiss"
            Layout.horizontalStretchFactor: 1
            Layout.fillWidth: true
            onClicked: function () {
                root.notification.notification.dismiss();
            }
        }
    }
}
