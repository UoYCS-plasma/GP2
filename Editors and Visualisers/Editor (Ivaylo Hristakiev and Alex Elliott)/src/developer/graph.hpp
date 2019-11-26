/*!
 * \file
 */
#ifndef GRAPH_HPP
#define GRAPH_HPP

#include "gpfile.hpp"
#include "global.hpp"

// This includes "node.hpp" by proxy
#include "edge.hpp"
#include "parsertypes.hpp"
#include <vector>
#include <QRect>

namespace Developer {

class Graph : public GPFile
{
    Q_OBJECT

public:
    Graph(const QString &graphPath = QString(),
          bool autoInitialise = true,
          QObject *parent = 0,
          bool isRuleGraph = false);
    Graph(const graph_t &inputGraph, QObject *parent = 0,
          bool isRuleGraph = false);

    bool save();
    bool saveAs(const QString &filePath);
    bool exportTo(const QString &filePath, GraphTypes outputType);

    bool open();

    QRect canvas() const;
    Node *node(const QString &id) const;
    Edge *edge(const QString &id) const;
    bool hasEdgeFrom(const QString &id) const;
    bool hasEdgeTo(const QString &id) const;
    bool hasEdgeFromTo(const QString &sourceId, const QString &targetId) const;
    std::vector<Node *> nodes() const;
    std::vector<Edge *> edges(const QString &id = QString()) const;
    std::vector<Edge *> edgesFrom(const QString &id) const;
    std::vector<Edge *> edgesTo(const QString &id) const;
    std::vector<Edge *> edgesFromTo(const QString &sourceId, const QString &targetId) const;

    QStringList nodeIdentifiers() const;
    QStringList edgeIdentifiers() const;

    bool contains(const QString &id) const;
    bool containsNode(const QString &id) const;
    bool containsEdge(const QString &id) const;

    bool isRuleGraph() const;

    QString toString(int outputType = DefaultGraph, bool keepLayout = true);
    QString toGxl(bool keepLayout = true) const;
    QString toDot(bool keepLayout = true) const;
    QString toAlternative();
    QString toLaTeX() const;


    QString newNodeId();
    QString newEdgeId();

signals:
    void graphChanged();
    void nodeAdded(Node *n);
    void edgeAdded(Edge *e);
    void nodeRemoved(QString id);
    void edgeRemoved(QString id);
    void openComplete();

public slots:
    void setCanvas(const QRect &rect);
    Node *addNode(const QString &id, const QString &label = QString(), const QString &mark = QString(), bool isRoot = false, bool isInterface = false, const QPointF &pos = QPointF());
    //Node *addNode(const QString &label = QString(), const QString &mark = QString(), bool isRoot = false, const QPointF &pos = QPointF());
    Edge *addEdge(const QString &id, Node *from, Node *to, const QString &label = QString(), const QString &mark = QString(), bool isBidirectional = false);
    //Edge *addEdge(Node *from, Node *to, const QString &label = QString(), const QString &mark = QString(), bool isBidirectional = false);

    /*!
     * \brief Remove the node with the given ID from the graph if it exists
     *
     * Node removal can be permissive or strict, with the default being
     * permissive. When deletion is permissive a node which has inbound or
     * outbound edges causes deletion to cascade to those edges. If deletion is
     * strict then the removal will fail and return false unless the node has no
     * incident edges.
     *
     * \param   id      The ID of the node to remove
     * \param   strict  True if removal should fail when the node has incident
     *      edges, false if any incident edges should be automatically rmeoved
     * \return  True if the removal was successful, false otherwise
     */
    bool removeNode(const QString &id, bool strict = false);
    bool removeEdge(const QString &id);

protected slots:
    void trackChange();

protected:
    // Protected member functions
    bool openGraphT(const graph_t &inputGraph);

    // Protected member variables
    int _nodeIdCounter;
    int _edgeIdCounter;
    QRect _canvas;
    bool _isRuleGraph;
    std::vector<Node *> _nodes;
    std::vector<Edge *> _edges;

    // Some convenience typedefs (not going to tie in C++11 as a requirement)
    typedef std::vector<Node *>::iterator nodeIter;
    typedef std::vector<Edge *>::iterator edgeIter;
    typedef std::vector<Node *>::const_iterator nodeConstIter;
    typedef std::vector<Edge *>::const_iterator edgeConstIter;
};

}

#endif // GRAPH_HPP
