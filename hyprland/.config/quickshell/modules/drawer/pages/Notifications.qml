pragma ComponentBehavior: Bound
import QtQuick
import QtQuick.Controls
import qs.modules.widgets.notifications

BasePage {
    id: root
    title: "Notifications"

    ScrollView {
        height: root.height

        Notifications {
            anchors.fill: parent
        }
    }
}
