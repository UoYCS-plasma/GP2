/*!
 * \file
 */
#include "prettytabmenu.hpp"

#include "prettytabheader.hpp"
#include <QDebug>

namespace Developer {

PrettyTabMenu::PrettyTabMenu(const QString &title, QWidget *parent)
    : QWidget(parent)
    , _title(title)
    , _tabCount(0)
{
    _layout = new QVBoxLayout(this);
    _layout->setMargin(0);
    _layout->setSpacing(0);

    // If we have been passed a title make an item for it and increase the tab
    // count as if it was a tab (though it won't respond to clicks)
    if(!title.isEmpty())
    {
        PrettyTabHeader *label = new PrettyTabHeader(title, this);
        _layout->insertWidget(0, label);
        ++_tabCount;
    }

    _mapper = new QSignalMapper(this);
    connect(_mapper, SIGNAL(mapped(int)), this, SLOT(tabTriggered(int)));
}

PrettyTab *PrettyTabMenu::tab(int index) const
{
    if(index < _tabs.size())
        return _tabs[index];
    else
        return 0;
}

int PrettyTabMenu::addTab(const QString &name, const QIcon &icon)
{
    // Insert the new tab
    PrettyTab *tab = new PrettyTab(name, icon, this);
    _layout->insertWidget(_tabCount, tab);

    // Check if this is the first tab in the "default" menu, if it is then we
    // need to inform it that it is a special case in display terms
    if(_tabs.size() == 0 && (_title.isEmpty() || _title == "default"))
    {
        tab->setTop(true);
    }

    connect(tab, SIGNAL(pressed()), _mapper, SLOT(map()));
    _mapper->setMapping(tab, _tabCount);

    _tabs << tab;

    // return _tabCount++
    int ret = _tabCount;
    ++_tabCount;
    return ret;
}

QString PrettyTabMenu::title() const
{
    return _title;
}

bool PrettyTabMenu::contains(int index) const
{
    if(index < 0 || index >= _tabCount)
        return false;
    return true;
}

void PrettyTabMenu::clearSelection()
{
    for(int i = 0; i < _tabs.size(); ++i)
        _tabs.at(i)->clearSelection();
}

void PrettyTabMenu::setCurrentIndex(int index)
{
    // Update the current tab
    int internalIndex = index;
    if(!_title.isEmpty())
        --internalIndex;

    _tabs.at(internalIndex)->setSelected(true);
}

void PrettyTabMenu::tabTriggered(int index)
{
    // Update the current tab
    int internalIndex = index;
    if(!_title.isEmpty())
        --internalIndex;

    clearSelection();
    _tabs.at(internalIndex)->setSelected(true);

    // Emit a signal announcing this change of state
    if(_title.isEmpty())
        emit tabTriggered(QString("default"), index);
    else
        emit tabTriggered(_title, index);
}

}
