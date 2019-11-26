/*!
 * \file
 */
#ifndef GRAPHITEM_HPP
#define GRAPHITEM_HPP

#include <QGraphicsObject>

#include "global.hpp"

namespace Developer {

class GraphItem : public QGraphicsObject
{
    Q_OBJECT

public:
    enum ItemState
    {
        GraphItem_Normal,
        GraphItem_New,
        GraphItem_Deleted,
        GraphItem_Invalid
    };

    explicit GraphItem(const QString &itemId,
                       const QString &itemLabel = QString(),
                       const QString &itemType = QString(),
                       QGraphicsItem *parent = 0);

    QString id() const;
    QString label() const;
    QString itemType() const;
    ItemState itemState() const;

    void setId(const QString &itemId);
    void setLabel(const QString &itemLabel);
    void setItemState(ItemState state);

signals:
    void idChanged(QString oldId, QString newId);

protected:
    QString _id;
    QString _label;
    QString _itemType;
    ItemState _itemState;
};

}

#endif // GRAPHITEM_HPP
