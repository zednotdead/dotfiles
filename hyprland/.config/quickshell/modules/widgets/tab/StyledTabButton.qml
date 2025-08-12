import QtQuick
import QtQuick.Controls
import qs.config
import qs.modules.widgets

TabButton {
    id: control

    contentItem: StyledText {
        text: control.text
        opacity: enabled ? 1.0 : 0.3
        color: Theme.foreground
        horizontalAlignment: Text.AlignHCenter
        verticalAlignment: Text.AlignVCenter
        elide: Text.ElideRight
    }
    background: Rectangle {
        implicitWidth: 100
        implicitHeight: 40
        opacity: enabled ? 1 : 0.3
        color: {
            const color = Theme.barBackground;
            let dial = 0
            if (control.down) {
                dial = 2
            } else if (control.hovered) {
                dial = 1.4
            } else {
                dial = 1
            }

            if (control.checked) {
                dial *= 1.3
            }

            return Qt.lighter(color, dial);
        }
        border.width: 0
        radius: 2
    }
}
