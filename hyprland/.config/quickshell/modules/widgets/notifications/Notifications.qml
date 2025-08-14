import QtQuick
import QtQuick.Layouts
import qs.services.notifications

ColumnLayout {
    spacing: 10
    Repeater {
        model: Notifs.list
        Notification {
            required property Notifs.Notif modelData
            Layout.fillWidth: true
            notification: modelData
            implicitWidth: parent.width
        }
    }
}
