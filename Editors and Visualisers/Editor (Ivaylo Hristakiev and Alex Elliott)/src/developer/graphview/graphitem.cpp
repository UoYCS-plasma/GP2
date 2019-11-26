/*!
 * \file
 */
#include "graphitem.hpp"

namespace Developer {

GraphItem::GraphItem(const QString &itemId, const QString &itemLabel, const QString &itemType,
                     QGraphicsItem *parent)
    : QGraphicsObject(parent)
    , _id(itemId)
    , _label(itemLabel)
    , _itemType(itemType)
    , _itemState(GraphItem_Normal)
{
}

QString GraphItem::id() const
{
    return _id;
}

QString GraphItem::label() const
{
    return _label;
}

QString GraphItem::itemType() const
{
    return _itemType;
}

GraphItem::ItemState GraphItem::itemState() const
{
    return _itemState;
}

void GraphItem::setId(const QString &itemId)
{
    if(itemId != _id)
    {
        emit idChanged(_id, itemId);
        _id = itemId;
    }
    update();
}

void GraphItem::setLabel(const QString &itemLabel)
{
    _label = itemLabel;
    update();
}

void GraphItem::setItemState(ItemState state)
{
    _itemState = state;
    update();
}

}
