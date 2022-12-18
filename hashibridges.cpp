//
// Hashiwokakero Solver - engine
// by d/x, Spring 2022, Daniel Koziarski
//

#include <cassert>
#include <cstddef>
#include <cstdio>
#include <cstdarg>

#include <vector>
#include <array>
#include <queue>
#include <stack>
#include <iterator>
#include <algorithm>

#include "hashibridges.h"

std::stack<HashiNode::Bridge> *HashiNode::sUndoStack_ = nullptr;
int HashiNode::sObjCounter_ = 0;
bool (*HashiNode::sSA_NeighboursSwitch[4])( HashiNode *node,
                                            Neighbourhood *others) = {
                                              SA_1_NeighbourCase,
                                              SA_2_NeighboursCase,
                                              SA_3_NeighboursCase,
                                              SA_4_NeighboursCase };
bool (*HashiNode::sECA_NeighboursSwitch[3])( HashiNode *node,
                                             Neighbour *others[], ECA_kit *kit ) = {
                                               ECA_2_NeighboursCase,
                                               ECA_3_NeighboursCase,
                                               ECA_4_NeighboursCase };

HashiNode::Bridge::Bridge( HashiNode *node, HashiNode *other, char bridgeType ) :
  node_ { node },
  other_ { other },
  bridgeType_ { bridgeType } {
}  // -------------------------------------------------------------------------------------------

HashiNode::HashiNode() :
  row_ { 0 },
  col_ { 0 },
  bridgesWanted_ { 0 },
  bridgesFound_ { 0 },
  bridgesAbsent_ { 0 },
  floodFlag_ { 0 },
  //BFSvisitedFlag_ { 0 },
  hashiPlay_ { nullptr } {
  sObjCounter_++;
  //printf( "HashiNode() DEFAULT constructor #%d,  unexpected behaviour !!!\n", _objCounter );
}  // -------------------------------------------------------------------------------------------

HashiNode::HashiNode( size_t row, size_t col, int bridgesWanted, HashiPlay *hashiPlay ) :
  row_ { row },
  col_ { col },
  bridgesWanted_ { bridgesWanted },
  bridgesFound_ { 0 },
  bridgesAbsent_ { bridgesWanted },
  floodFlag_ { 0 },
  //BFSvisitedFlag { 0 },
  hashiPlay_ { hashiPlay } {
  sObjCounter_++;
  //printf( "HashiNode(r,c,brigesNeeded,hashiPlay *) constructor #%d\n", _objCounter );
}  // -------------------------------------------------------------------------------------------

HashiNode::~HashiNode() {
  //printf( "~HashiNode() DEFAULT DEstructor #%d\n", _objCounter );
  sObjCounter_--;
}  // -------------------------------------------------------------------------------------------

HashiNode *HashiNode::initHashiNode( size_t row, size_t col, int bridgesWanted, HashiPlay *hashiPlay ) {
  row_            = row;
  col_            = col;
  bridgesWanted_  = bridgesWanted;
  bridgesFound_   = 0;
  bridgesAbsent_  = bridgesWanted - bridgesFound_;
  floodFlag_      = 0;
  //BFSvisitedFlag_ = 0;
  //hashiPlay_    = hashiPlay;

  return (this);
}  // -------------------------------------------------------------------------------------------

inline uchar *HashiNode::getHashiFieldLocation() const {
  return hashiPlay_->getHashiOutputField() + (row_ * hashiPlay_->getHashiFieldCols()) + col_;
}  // -------------------------------------------------------------------------------------------

inline size_t HashiNode::getHashiFieldCols() const { return hashiPlay_->getHashiFieldCols(); }
inline size_t HashiNode::getHashiFieldRows() const { return hashiPlay_->getHashiFieldRows(); }

inline int HashiNode::getHashiMapEastwardStep( void ) { return  1; }
inline int HashiNode::getHashiMapWestwardStep( void ) { return -1; }
inline int HashiNode::getHashiMapNorthwardStep( void ) const { return -(int) hashiPlay_->getHashiFieldCols(); }
inline int HashiNode::getHashiMapSouthwardStep( void ) const { return  (int) hashiPlay_->getHashiFieldCols(); }

void HashiNode::initNeighbour( Neighbour *neighbour ) {

  assert( neighbour != nullptr );

  neighbour->node          = nullptr;
  neighbour->bridgeWidth   = 0;
  neighbour->bridgeLength  = 0;
  neighbour->maxNewBridges = 0;

}  // -------------------------------------------------------------------------------------------
void HashiNode::initNeighbour( Neighbour *neighbour, const HashiNode *node, int bridgeWidth, int bridgeLength ) {

  assert( neighbour != nullptr  &&  node != nullptr );

  neighbour->node          = const_cast <HashiNode *> (node);
  neighbour->bridgeWidth   = bridgeWidth;
  neighbour->bridgeLength  = bridgeLength;
  neighbour->maxNewBridges = maxNewBridges( neighbour );

}  // -------------------------------------------------------------------------------------------


void HashiNode::getEastNeighbour ( Neighbour *neighbour ) const {
  const uchar *hf     = getHashiFieldLocation();
  const uchar *hfEdge = hf + (getHashiFieldCols() - col_);  // scan limiter
  int          hfStep = getHashiMapEastwardStep();

  int bridgeLength = 0;
  do {
    hf += hfStep;
    if (hf == hfEdge  ||  is_Any_VerticalBridge( *hf )) {
      initNeighbour( neighbour );
      return;                                    // neighbour node unavailable
    }
    if (isNode( *hf )) {                         // neigbour node?
      initNeighbour( neighbour,
                     hashiPlay_->getAllNodes()[hf - hashiPlay_->getHashiOutputField()],
                     getBridgeWidth( hf[-hfStep] ),
                     bridgeLength );
      return;                                    // neighbour found
    }
    bridgeLength++;
  } while (isEmpty( *hf )  ||  is_Any_HorizontalBridge( *hf ));

  assert( 0 );
}  // -------------------------------------------------------------------------------------------
void HashiNode::getWestNeighbour ( Neighbour *neighbour ) const {
  const uchar *hf     = getHashiFieldLocation();
  const uchar *hfEdge = hf - (col_ + 1);;        // scan limiter
  int          hfStep = getHashiMapWestwardStep();

  int bridgeLength = 0;
  do {
    hf += hfStep;
    if (hf == hfEdge  ||  is_Any_VerticalBridge( *hf )) {
      initNeighbour( neighbour );
      return;                                    // neighbour node unavailable
    }
    if (isNode( *hf )) {                         // neigbour node?
      initNeighbour( neighbour,
                     hashiPlay_->getAllNodes()[hf - hashiPlay_->getHashiOutputField()],
                     getBridgeWidth( hf[-hfStep] ),
                     bridgeLength );
      return;                                    // neighbour found
    }
    bridgeLength++;
  } while (isEmpty( *hf )  ||  is_Any_HorizontalBridge( *hf ));

  assert( 0 );
}  // -------------------------------------------------------------------------------------------
void HashiNode::getNorthNeighbour( Neighbour *neighbour ) const {
  const uchar *hf     = getHashiFieldLocation();
  const uchar *hfEdge = hf - ((row_ + 1) * getHashiFieldCols());  // scan limiter
  int          hfStep = getHashiMapNorthwardStep();

  int bridgeLength = 0;
  do {
    hf += hfStep;
    if (hf == hfEdge  ||  is_Any_HorizontalBridge( *hf )) {
      initNeighbour( neighbour );
      return;                                    // neighbour node unavailable
    }
    if (isNode( *hf )) {                         // neigbour node?
      initNeighbour( neighbour,
                     hashiPlay_->getAllNodes()[hf - hashiPlay_->getHashiOutputField()],
                     getBridgeWidth( hf[-hfStep] ),
                     bridgeLength );
      return;                                    // neighbour found
    }
    bridgeLength++;
  } while (isEmpty( *hf )  ||  is_Any_VerticalBridge( *hf ));

  assert( 0 );
}  // -------------------------------------------------------------------------------------------
void HashiNode::getSouthNeighbour( Neighbour *neighbour ) const {
  const uchar *hf     = getHashiFieldLocation();
  const uchar *hfEdge = hf + ((getHashiFieldRows() - row_) * getHashiFieldCols());  // scan limiter
  int          hfStep = getHashiMapSouthwardStep();

  int bridgeLength = 0;
  do {
    hf += hfStep;
    if (hf == hfEdge  ||  is_Any_HorizontalBridge( *hf )) {
      initNeighbour( neighbour );
      return;                                    // neighbour node unavailable
    }
    if (isNode( *hf )) {                         // neigbour node?
      initNeighbour( neighbour,
                     hashiPlay_->getAllNodes()[hf - hashiPlay_->getHashiOutputField()],
                     getBridgeWidth( hf[-hfStep] ),
                     bridgeLength );
      return;                                    // neighbour found
    }
    bridgeLength++;
  } while (isEmpty( *hf )  ||  is_Any_VerticalBridge( *hf ));

  assert( 0 );
}  // -------------------------------------------------------------------------------------------

void HashiNode::exploreNeighbourhood( Neighbourhood *others ) const {

  assert( bridgesAbsent_ >= 0 );       // accept also bridgesAbsent_ == 0, required by deepAnalysis()

  getEastNeighbour ( &others->east );  // look around, determine possible bridges
  getWestNeighbour ( &others->west );
  getNorthNeighbour( &others->north );
  getSouthNeighbour( &others->south );

#if 0
  if (others->east.node != nullptr)
    printf( "Found neighbour EAST : (%u, %u) @[%2zu,%-2zu] (maxNewBridges: %d, length: %d, width: %d)\n",
            others->east.node->_bridgesWanted, others->east.node->_bridgesFound,
            others->east.node->_row, others->east.node->_col,
            others->east.maxNewBridges, others->east.bridgeLength, others->east.bridgeWidth );
  if (others->west.node != nullptr)
    printf( "Found neighbour WEST : (%u, %u) @[%2zu,%-2zu] (maxNewBridges: %d, length: %d, width: %d)\n",
            others->west.node->_bridgesWanted, others->west.node->_bridgesFound,
            others->west.node->_row, others->west.node->_col,
            others->west.maxNewBridges, others->west.bridgeLength, others->west.bridgeWidth );
  if (others->north.node != nullptr)
    printf( "Found neighbour NORTH: (%u, %u) @[%2zu,%-2zu] (maxNewBridges: %d, length: %d, width: %d)\n",
            others->north.node->_bridgesWanted, others->north.node->_bridgesFound,
            others->north.node->_row, others->north.node->_col,
            others->north.maxNewBridges, others->north.bridgeLength, others->north.bridgeWidth );
  if (others->south.node != nullptr)
    printf( "Found neighbour SOUTH: (%u, %u) @[%2zu,%-2zu] (maxNewBridges: %d, length: %d, width: %d)\n",
            others->south.node->_bridgesWanted, others->south.node->_bridgesFound,
            others->south.node->_row, others->south.node->_col,
            others->south.maxNewBridges, others->south.bridgeLength, others->south.bridgeWidth );
#endif

#if 1
  if (bridgesWanted_ == 1)                       // reject connections of type: 1-1
    for (auto &neighbour : others->allAround)    // east, west, north, south
      if (neighbour.node != nullptr  &&  neighbour.node->bridgesWanted_ == 1)
        neighbour.node = nullptr, neighbour.maxNewBridges = 0;
#endif
#if 1
  if (bridgesWanted_ == 2)                       // reject connections of type: 2=2
    for (auto &neighbour : others->allAround)    // east, west, north, south
      if (neighbour.node != nullptr  &&  neighbour.node->bridgesWanted_ == 2)
        if (neighbour.maxNewBridges == 2)
          neighbour.maxNewBridges = 1;
#endif

  int neighbourCount  = 0;                       // merge explored results
  int bridgesPossible = 0;

  for (auto &neighbour : others->allAround)      // east, west, north, south
    if (neighbour.maxNewBridges != 0)
      neighbourCount += 1, bridgesPossible += neighbour.maxNewBridges;

  others->neighbourCount  = neighbourCount;
  others->bridgesPossible = bridgesPossible;
}  // -------------------------------------------------------------------------------------------
void HashiNode::getConnectedNodes   ( Neighbourhood *others ) const {

  assert( this != nullptr  &&  others != nullptr );

  getEastNeighbour ( &others->east );            // look around for neighbour nodes
  getWestNeighbour ( &others->west );
  getNorthNeighbour( &others->north );
  getSouthNeighbour( &others->south );

  for (auto &neighbour : others->allAround)
    if (neighbour.bridgeWidth == 0)
      neighbour.node = nullptr;                  // ignore all NOT connected neighbours

  others->neighbourCount =
  others->bridgesPossible = -1;                  // surely illegal, but fixed values

}  // -------------------------------------------------------------------------------------------

void HashiNode::protocolBridgesMadeOnNodes( Neighbour *other, int bridgesMade ) {

  assert( bridgesWanted_              >= bridgesFound_ + bridgesMade );
  assert( other->node->bridgesWanted_ >= other->node->bridgesFound_ + bridgesMade );

  bridgesFound_              += bridgesMade;
  bridgesAbsent_              = bridgesWanted_ - bridgesFound_;
  other->node->bridgesFound_ += bridgesMade;
  other->node->bridgesAbsent_ = other->node->bridgesWanted_ - other->node->bridgesFound_;
  other->bridgeWidth         += bridgesMade;

  assert( bridgesFound_  >= 0  &&  bridgesFound_  <= bridgesWanted_ );
  assert( bridgesAbsent_ >= 0  &&  bridgesAbsent_ <= bridgesWanted_ );

  assert( other->node->bridgesFound_  >= 0  &&  other->node->bridgesFound_  <= other->node->bridgesWanted_ );
  assert( other->node->bridgesAbsent_ >= 0  &&  other->node->bridgesAbsent_ <= other->node->bridgesWanted_ );

  assert( other->bridgeWidth >= 0  &&  other->bridgeWidth <= 2 );

}  // -------------------------------------------------------------------------------------------

void HashiNode::makeHorizontalBridge( Neighbour *neighbour, char bridgeType ) {

  assert( neighbour != nullptr  &&  neighbour->node != nullptr );
  assert( row_ == neighbour->node->row_ );
  assert( neighbour->bridgeLength > 0 );
  assert( bridgeType == 0  &&  neighbour->bridgeWidth >= 0  &&  neighbour->bridgeWidth <= 2   ||
          bridgeType  > 0  &&  neighbour->bridgeWidth >= 0  &&  neighbour->bridgeWidth  < 2   ||
          bridgeType  < 0  &&  neighbour->bridgeWidth <= 2  &&  neighbour->bridgeWidth  > 0 );
  assert( bridgesWanted_ == bridgesFound_ + bridgesAbsent_ );
  assert( neighbour->node->bridgesWanted_ == neighbour->node->bridgesFound_ + neighbour->node->bridgesAbsent_ );

  uchar       *hf     = getHashiFieldLocation();
  const uchar *hfEnd  = neighbour->node->getHashiFieldLocation();
  int          hfStep = (col_ < neighbour->node->col_) ? getHashiMapEastwardStep()
                                                       : getHashiMapWestwardStep();
  uchar        bridgeKey;

  switch (bridgeType) {
  case 0:                                        // nothing to do
    return;
  case -cDoubleBridge:                           // drop double bridge
    if (neighbour->bridgeWidth == 2) {
      assert( isDoubleHorizontalBridge( hf[hfStep] ) );
      bridgeKey = cNoBridgeCode;
    } else
      assert( 0 );
    break;
  case -cSingleBridge:                           // drop single bridge
    if (neighbour->bridgeWidth > 0) {
      assert( is_Any_HorizontalBridge( hf[hfStep] ) );
      bridgeKey = (neighbour->bridgeWidth == 2) ? cSingleHorizontalBridgeCode : cNoBridgeCode;
    } else
      assert( 0 );
    break;
  case cSingleBridge:                            // add single bridge
    if (neighbour->bridgeWidth < 2) {
      assert( isEmpty( hf[hfStep] )  ||  isSingleHorizontalBridge( hf[hfStep] ) );
      bridgeKey = (neighbour->bridgeWidth == 0) ? cSingleHorizontalBridgeCode : cDoubleHorizontalBridgeCode;
    } else
      assert( 0 );
    break;
  case cDoubleBridge:                            // make double bridge from single bridge too
    if (neighbour->bridgeWidth < 2) {
      assert( isEmpty( hf[hfStep] )  ||  isSingleHorizontalBridge( hf[hfStep] ) );
      bridgeKey = cDoubleHorizontalBridgeCode;
      bridgeType = (neighbour->bridgeWidth == 1) ? cSingleBridge : bridgeType;
    } else
      assert( 0 );
    break;
  default:
    assert( 0 );
  }
  for (hf += hfStep; hf != hfEnd; hf += hfStep)
    *hf = bridgeKey;

  protocolBridgesMadeOnNodes( neighbour, bridgeType );
}  // -------------------------------------------------------------------------------------------
void HashiNode::makeVerticalBridge  ( Neighbour *neighbour, char bridgeType ) {

  assert( neighbour != nullptr  &&  neighbour->node != nullptr );
  assert( col_ == neighbour->node->col_ );
  assert( neighbour->bridgeLength > 0 );
  assert( bridgeType == 0  &&  neighbour->bridgeWidth >= 0  &&  neighbour->bridgeWidth <= 2   ||
          bridgeType  > 0  &&  neighbour->bridgeWidth >= 0  &&  neighbour->bridgeWidth  < 2   ||
          bridgeType  < 0  &&  neighbour->bridgeWidth <= 2  &&  neighbour->bridgeWidth  > 0 );
  assert( bridgesWanted_ == bridgesFound_ + bridgesAbsent_ );
  assert( neighbour->node->bridgesWanted_ == neighbour->node->bridgesFound_ + neighbour->node->bridgesAbsent_ );

  uchar       *hf     = getHashiFieldLocation();
  const uchar *hfEnd  = neighbour->node->getHashiFieldLocation();
  int          hfStep = (row_ < neighbour->node->row_) ? getHashiMapSouthwardStep()
                                                       : getHashiMapNorthwardStep();
  uchar        bridgeKey;

  switch (bridgeType) {
  case 0:                                        // nothing to do
    return;
  case -cDoubleBridge:                           // drop double bridge
    if (neighbour->bridgeWidth == 2) {
      assert( isDoubleVerticalBridge( hf[hfStep] ) );
      bridgeKey = cNoBridgeCode;
    } else
      assert( 0 );
    break;
  case -cSingleBridge:                           // drop single bridge
    if (neighbour->bridgeWidth > 0) {
      assert( is_Any_VerticalBridge( hf[hfStep] ) );
      bridgeKey = (neighbour->bridgeWidth == 2) ? cSingleVerticalBridgeCode : cNoBridgeCode;
    } else
      assert( 0 );
    break;
  case cSingleBridge:                            // add single bridge
    if (neighbour->bridgeWidth < 2) {
      assert( isEmpty( hf[hfStep] )  ||  isSingleVerticalBridge( hf[hfStep] ) );
      bridgeKey = (neighbour->bridgeWidth == 0) ? cSingleVerticalBridgeCode : cDoubleVerticalBridgeCode;
    } else
      assert( 0 );
    break;
  case cDoubleBridge:                            // make double bridge from single bridge too
    if (neighbour->bridgeWidth < 2) {
      assert( isEmpty( hf[hfStep] )  ||  isSingleVerticalBridge( hf[hfStep] ) );
      bridgeKey = cDoubleVerticalBridgeCode;
      bridgeType = (neighbour->bridgeWidth == 1) ? cSingleBridge : bridgeType;
    } else
      assert( 0 );
    break;
  default:
    assert( 0 );
  }
  for (hf += hfStep; hf != hfEnd; hf += hfStep)
    *hf = bridgeKey;

  protocolBridgesMadeOnNodes( neighbour, bridgeType );
}  // -------------------------------------------------------------------------------------------

void HashiNode::makeBridge( Neighbour *neighbour, char bridgeType ) {
  void (HashiNode:: *makeBridge) (Neighbour *neighbour, char bridgeType);

  assert( this != nullptr  &&  neighbour != nullptr  &&  neighbour->node != nullptr );

  if (row_ == neighbour->node->row_)
    makeBridge = &HashiNode::makeHorizontalBridge;
  else {
    assert( col_ == neighbour->node->col_ );
    makeBridge = &HashiNode::makeVerticalBridge;
  }
  if (bridgeType == cMaxNewBridges)
    bridgeType = neighbour->maxNewBridges;
  else if (bridgeType == -cMaxNewBridges)
    bridgeType = -neighbour->maxNewBridges;

  (this->*makeBridge)( neighbour, bridgeType );  // finally call proper method

  std::stack<Bridge> *undoStack = sUndoStack_;
  if (undoStack != nullptr)                      // engaged "undo" engine?
    undoStack->emplace( this, neighbour->node, bridgeType );
}  // -------------------------------------------------------------------------------------------
void HashiNode::makeBridge( Neighbour *n1, Neighbour *n2, char bridgeType ) {

  makeBridge( n1, bridgeType );
  makeBridge( n2, bridgeType );

}  // -------------------------------------------------------------------------------------------
void HashiNode::makeBridge( Neighbour *n1, Neighbour *n2, Neighbour *n3, char bridgeType ) {

  makeBridge( n1, bridgeType );
  makeBridge( n2, bridgeType );
  makeBridge( n3, bridgeType );

}  // -------------------------------------------------------------------------------------------

void HashiNode::dropBridge( Neighbour *neighbour, char bridgeType ) {
#if 1
  makeBridge( neighbour, -bridgeType );
#else
  void (HashiNode:: *makeBridge) (Neighbour *neighbour, char bridgeType);

  assert( this != nullptr  &&  neighbour != nullptr  &&  neighbour->node != nullptr );

  if (_row == neighbour->node->_row)
    makeBridge = &HashiPlay::makeHorizontalBridge;
  else {
    assert( _col == neighbour->node->_col );
    makeBridge = &HashiPlay::makeVerticalBridge;
  }
  (this->*makeBridge)( neighbour, -bridgeType );           // finally call proper method
#endif
}  // -------------------------------------------------------------------------------------------
void HashiNode::dropBridge( Neighbour *n1, Neighbour *n2, char bridgeType ) {

  makeBridge( n1, -bridgeType );
  makeBridge( n2, -bridgeType );

}  // -------------------------------------------------------------------------------------------
void HashiNode::dropBridge( Neighbour *n1, Neighbour *n2, Neighbour *n3, char bridgeType ) {

  makeBridge( n1, -bridgeType );
  makeBridge( n2, -bridgeType );
  makeBridge( n3, -bridgeType );

}  // -------------------------------------------------------------------------------------------

void HashiNode::undoBridge( const Bridge *bridge ) {
  // disassembly any bridge according to data obtained from the undo-stack

  assert( bridge != nullptr  &&  bridge->node_ != nullptr  &&  bridge->other_ != nullptr );
  assert( bridge->bridgeType_ == cSingleBridge  ||  bridge->bridgeType_ == cDoubleBridge );

  HashiNode *node = bridge->node_;
  uchar     *hf   = node->getHashiFieldLocation();
  int        hfStep;

  if (node->row_ == bridge->other_->row_)
    hfStep = (node->col_ < bridge->other_->col_) ? getHashiMapEastwardStep()
                                                 : getHashiMapWestwardStep();
  else {
    assert( node->col_ == bridge->other_->col_ );
    hfStep = (node->row_ < bridge->other_->row_) ? node->getHashiMapSouthwardStep()
                                                 : node->getHashiMapNorthwardStep();
  }
  Neighbour neighbour = {              // neighbour's data for make/dropBridge() call:
    bridge->other_,                     // HashiNode *node
    getBridgeWidth( hf[hfStep] ),      // int bridgeWidth
    1,                                 // int bridgeLength, any value > 0 is Ok
    -1                                 // int maxNewBridges, not relevant for make/dropBridge()
  };
  assert( node->sUndoStack_ == nullptr );
  node->makeBridge( &neighbour, -bridge->bridgeType_ );

}  // -------------------------------------------------------------------------------------------

bool HashiNode::makeSingleBridges    ( Neighbourhood *others ) {
  // if connection is empty, then sigle bridge will be build
  // if connection is already a single bridge, then it will be replaced by double bridge

  assert( others != nullptr );

  for (auto &neighbour : others->allAround)      // east, west, north, south
    if (neighbour.maxNewBridges != 0)
      makeBridge( &neighbour, cSingleBridge );
  //hashiPlay_->printHashiField();

  return true;
}  // -------------------------------------------------------------------------------------------
bool HashiNode::makeDoubleBridges    ( Neighbourhood *others ) {

  assert( others != nullptr );

  for (auto &neighbour : others->allAround)      // east, west, north, south
    if (neighbour.maxNewBridges != 0)
      makeBridge( &neighbour, cDoubleBridge );
  //hashiPlay_->printHashiField();

  return true;
}  // -------------------------------------------------------------------------------------------
bool HashiNode::makeSureSingleBridges( Neighbourhood *others ) {
  // special cases:
  //  : if (neighbourCount == 2  &&  bridgesAbsent == 2  &&  bridgesPossible == 3)
  //  : if (neighbourCount == 3  &&  bridgesAbsent == 3  &&  bridgesPossible == 4)
  //  : if (neighbourCount == 3  &&  bridgesAbsent == 4  &&  bridgesPossible == 5)
  //  : if (neighbourCount == 4  &&  bridgesAbsent == 4  &&  bridgesPossible == 5)
  //  : if (neighbourCount == 4  &&  bridgesAbsent == 5  &&  bridgesPossible == 6)
  //  : if (neighbourCount == 4  &&  bridgesAbsent == 6  &&  bridgesPossible == 7)
  // then single bridges can be _surely_ build, but only wherever a double bridge is possible

  assert( others != nullptr );

  for (auto &neighbour : others->allAround)      // east, west, north, south
    if (neighbour.maxNewBridges == cDoubleBridge)
      makeBridge( &neighbour, cSingleBridge );
  //hashiPlay_->printHashiField();

  return true;
}  // -------------------------------------------------------------------------------------------
bool HashiNode::make_All_Bridges     ( Neighbourhood *others ) {
  // special case: if (bridgesAbsent == bridgesPossible) then build just all!

  assert( others != nullptr );

  for (auto &neighbour : others->allAround)      // east, west, north, south
    if (neighbour.maxNewBridges != 0)
      makeBridge( &neighbour, (neighbour.maxNewBridges == 2) ? cDoubleBridge : cSingleBridge );
  //hashiPlay_->printHashiField();

  return true;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::SA_1_NeighbourCase( HashiNode *node, Neighbourhood *others ) {
  assert( node->bridgesAbsent_ == node->bridgesWanted_ - node->bridgesFound_ );
  assert( others->neighbourCount == 1);
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= others->neighbourCount * 2 );

  if (node->bridgesAbsent_ == 1) {
    //printf( "ONE node@: %d bridgesPossible -> make ONE single bridge\n", others->bridgesPossible );
    return node->makeSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 2) {
    //printf( "ONE node@: %d bridgesPossible -> make ONE DOUBLE bridge\n", others->bridgesPossible );
    return node->makeDoubleBridges( others );
  }

  return false;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::SA_2_NeighboursCase( HashiNode *node, Neighbourhood *others ) {
  assert( node->bridgesAbsent_ == node->bridgesWanted_ - node->bridgesFound_ );
  assert( others->neighbourCount == 2);
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= others->neighbourCount * 2 );

  if (node->bridgesAbsent_ == 2  &&  others->bridgesPossible == 2) {
    //printf( "TWO nodes@: %d bridgesPossible -> make TWO single bridges\n", others->bridgesPossible );
    return node->makeSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 2  &&  others->bridgesPossible == 3) {
    //printf ("TWO nodes@: %d/%d possible/missed Bridges ---> caught \"1of2\" bridges!!\n",
    //        others->bridgesPossible, node->_bridgesAbsent);
    return node->makeSureSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 3) {
    //printf( "TWO nodes@: %d bridgesPossible -> make TWO single bridges\n", others->bridgesPossible );
    return node->makeSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 4) {
    //printf( "TWO nodes@: %d bridgesPossible -> make TWO DOUBLE bridges\n", others->bridgesPossible );
    return node->makeDoubleBridges( others );
  }

  return false;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::SA_3_NeighboursCase( HashiNode *node, Neighbourhood *others ) {
  assert( node->bridgesAbsent_ == node->bridgesWanted_ - node->bridgesFound_ );
  assert( others->neighbourCount == 3);
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= others->neighbourCount * 2 );

  if (node->bridgesAbsent_ == 3  &&  others->bridgesPossible == 3   ||
      node->bridgesAbsent_ == 4  &&  others->bridgesPossible == 4) {
    //printf( "THREE nodes@: %d/%d possible/missed Bridges ---> full make!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->make_All_Bridges( others );
  }

  if (node->bridgesAbsent_ == 3  &&  others->bridgesPossible == 4) {
    //printf( "THREE nodes@: %d/%d possible/missed Bridges ---> caught \"1of3\" bridges!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->makeSureSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 4  &&  others->bridgesPossible == 5) {
    //printf( "THREE nodes@: %d/%d possible/missed Bridges ---> caught \"2of4\" bridges!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->makeSureSingleBridges( others );
  }

  if (node->bridgesAbsent_ == 5) {
    //printf( "THREE nodes@: %d bridgesPossible = make _3_ single bridges\n", others->bridgesPossible );
    return node->makeSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 6) {
    //printf( "THREE nodes@: %d bridgesPossible = make _3_ DOUBLE bridges\n", others->bridgesPossible );
    return node->makeDoubleBridges( others );
  }

  return false;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::SA_4_NeighboursCase( HashiNode *node, Neighbourhood *others ) {
  assert( node->bridgesAbsent_ == node->bridgesWanted_ - node->bridgesFound_ );
  assert( others->neighbourCount == 4);
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= others->neighbourCount * 2 );

  if (node->bridgesAbsent_ == 4  &&  others->bridgesPossible == 4   ||
      node->bridgesAbsent_ == 5  &&  others->bridgesPossible == 5   ||
      node->bridgesAbsent_ == 6  &&  others->bridgesPossible == 6) {
    //printf( "FOUR nodes@: %d/%d possible/missed Bridges ---> full make!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->make_All_Bridges( others );
  }

  if (node->bridgesAbsent_ == 4  &&  others->bridgesPossible == 5) {
    //printf( "FOUR nodes@: %d/%d possible/missed Bridges ---> caught \"1of4\" bridges!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->makeSureSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 5  &&  others->bridgesPossible == 6) {
    //printf( "FOUR nodes@: %d/%d possible/missed Bridges ---> caught \"2of5\" bridges!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->makeSureSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 6  &&  others->bridgesPossible == 7) {
    //printf( "FOUR nodes@: %d/%d possible/missed Bridges ---> caught \"3of6\" bridges!!\n",
    //        others->bridgesPossible, node->bridgesAbsent_ );
    return node->makeSureSingleBridges( others );
  }

  if (node->bridgesAbsent_ == 7) {
    //printf( "FOUR nodes@: %d bridgesPossible = make _4_ single bridges\n", others->bridgesPossible );
    return node->makeSingleBridges( others );
  }
  if (node->bridgesAbsent_ == 8) {
    //printf( "FOUR nodes@: %d bridgesPossible = make _4_ DOUBLE bridges\n", others->bridgesPossible );
    return node->makeDoubleBridges( others );
  }

  return false;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::staticAnalysis( bool *const errorCondition ) {

  assert( bridgesAbsent_ == bridgesWanted_ - bridgesFound_ );
  assert( bridgesAbsent_ >= 0 );

  Neighbourhood others = {};
  exploreNeighbourhood( &others );

  if (others.bridgesPossible < bridgesAbsent_  ||
      others.bridgesPossible < others.neighbourCount  ||
      others.neighbourCount  < 1  ||  others.neighbourCount > 4) {
    //hashiPlay_->printHashiField( "HashiField in error condition around NODE: (%u - %u = %d) "
    //                             "@[%2zu,%-2zu] :: neighbourCnt: %d, possible: %d\n",
    //                             bridgesWanted_, bridgesFound_, bridgesAbsent_, row_, col_,
    //                             others.neighbourCount, others.bridgesPossible );
    *errorCondition = true;
    return false;
  }
  assert( others.bridgesPossible >= bridgesAbsent_  &&
          others.bridgesPossible >= others.neighbourCount );

  assert( others.neighbourCount - 1 >= 0  &&
          others.neighbourCount - 1 < (int) elementsof( sSA_NeighboursSwitch ) );

  return (*sSA_NeighboursSwitch[others.neighbourCount - 1])( this, &others );
}  // -------------------------------------------------------------------------------------------


int  HashiNode::countBridgesPossibleAround( void ) const {

  Neighbourhood others = {};

  getEastNeighbour( &others.east );                        // look around for nodes
  getWestNeighbour( &others.west );
  getNorthNeighbour( &others.north );
  getSouthNeighbour( &others.south );

  int bridgesPossible = 0;
  for (const auto &neighbour : others.allAround)           // add possible bridges together
    bridgesPossible += neighbour.maxNewBridges;

  return bridgesPossible;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::anyNodeSeparated() const {
  // return true if any node in HashiPlay cannot be solved due to lack of possible bridges around it

  for (const auto &node : hashiPlay_->getTodoNodes())
    if (node.bridgesAbsent_ != 0) {
      int bridgesPossible = node.countBridgesPossibleAround();

      if (node.bridgesAbsent_ > bridgesPossible) {
        //hashiPlay_->printHashiField( "anyNodeSeparated():NODE: (%u - %u = %d) @[%2zu,%-2zu] :: "
        //                             "NOT enough bridgesPossible: %d --- error condition!\n",
        //                             node.bridgesWanted_, node.bridgesFound_, node.bridgesAbsent_,
        //                             node.row_, node.col_, bridgesPossible );
        return true;
      }
    }

  return false;    // correct solving possibility cannot be ruled out
}  // -------------------------------------------------------------------------------------------

void HashiNode::resetFloodFlags() const {

  for (auto &node : hashiPlay_->getTodoNodes())
    node.floodFlag_ = 0;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::isolatedGroup( bool rootCall ) const {
  // return true, if there is any isolated group, i.e the group consists of only SOLVED and together
  //              connected nodes. Hence these group cannot have further connections.
  //              Checking starts at current node, where the isolated group detection is higher. 
  static size_t nodeVisitedCount;

  if (rootCall) {                      // recursive tracing via Depth First Search algorithm
    nodeVisitedCount = 0;
    resetFloodFlags();
    //hashiPlay_->printHashiField();
  }
  //printf ("isolatedGroup(): entering && checking node (%u - %u = %d) @[%2zu,%-2zu]\n",
  //        _bridgesWanted, _bridgesFound, _bridgesAbsent, _row, _col);

  nodeVisitedCount += 1;
  floodFlag_ = 1;                      // against closed paths (meshes in hashiwokakero)

  assert( bridgesAbsent_ == 0 );       // starting node should be solved

  // highest probability for an Isolated Group of nodes
  // is for the group starting at current node object.
  bool isolated = true;
  Neighbourhood others = {};
  getConnectedNodes( &others );        // look around and check connected neighbours too

  for (const auto &neighbour : others.allAround) {         // east, west, north, south
    if (neighbour.node != nullptr  &&
        neighbour.node->floodFlag_ == 0)                   // flag test avoids endless recursion (meshes!)
      if (neighbour.node->bridgesAbsent_ != 0) {
        // node seems to be NOT isolated, because of found connection to an unsolved neighbour.
        // However found neighbour may be surrounded by further neighbours of type 1 only.
        // This kind of nodes (_bridgesWanted == 1) are always a separator between groups.
        // Hence the following test of neighbour's neighbourhood.
        Neighbourhood neighboursOthers = {};
        neighbour.node->exploreNeighbourhood( &neighboursOthers );
        bool typeOneNeighboursOnly = true;
        for (const auto &furtherNeighbour : neighboursOthers.allAround)
          if (furtherNeighbour.node != nullptr  &&         // further neigbour available?
              furtherNeighbour.node != neighbour.node  &&  // different from previous node? 
              furtherNeighbour.node->bridgesWanted_ != 1) {
            // neighbour.node is NOT isolated, because of found connection to an unsolved node, 
            // which (further) neighbours are not only of type 1 (bridgesWanted == 1).
            typeOneNeighboursOnly = false;
            break;
          }
        if (typeOneNeighboursOnly) {
          //printf( "isolatedGroup(): SEED node (%u - %u = %d) @[%2zu,%-2zu] :: "
          //        "node @[%2zu,%-2zu] is unsolved, but surrounded by type 1 nodes only !! !! !! !!\n",
          //        bridgesWanted_, bridgesFound_, bridgesAbsent_, row_, col_,
          //        neighbour.node->row_, neighbour.node->col_ );
        }  else
          isolated = false;
        //break; :: do NOT finish loop, to flood/visit all solved nodes in the current group
      } else if (neighbour.node->isolatedGroup( false ) == false) {
        // connected neighbour is solved, but NOT isolated, i.e. the group is NOT too
        isolated = false;
        //break; :: do NOT finish loop, to flood/visit all solved nodes in the current group
      }
  }
  if (rootCall == false)
    return isolated;

  if (isolated && nodeVisitedCount == hashiPlay_->getTodoNodes().size()) {
    //printf( "!!! isolatedGroup(): riddle seems to be solved, all nodes are within a single group.\n" );
    // Found isolated group may be the whole and solved(!) riddle (i.e. the only remaining group).
    // Such case should NOT be considered as isolated, because this is NOT an error condition.
    return false;
  }
  if (isolated == false) {
    // The group starting at current node object of initial method call (rootCall == true) 
    // could NOT be detected as an isolated group.
    // Hence try to detect any isolated group of nodes within the whole hashiwokakero riddle:
    for (const auto &node : hashiPlay_->getTodoNodes())
      if (node.bridgesAbsent_ == 0  &&           // solved node  AND
          node.floodFlag_ == 0)                  // not analysed yet?
        if (node.isolatedGroup( false )) {
          isolated = true;
          break;
        }
  }
  if (isolated) {
    //hashiPlay_->printHashiField( "isolatedGroup(): SEED node (%u - %u = %d) @[%2zu,%-2zu] :: "
    //                             "ISOLATED GROUP! --- error condition FOUND!\n",
    //                             bridgesWanted_, bridgesFound_, bridgesAbsent_, row_, col_ );
  }
  return isolated;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::advancedAnalysis_errorConditionTest( void ) {
  return (anyNodeSeparated() || isolatedGroup( true ));
}  // -------------------------------------------------------------------------------------------

bool HashiNode::hashiSolved( bool rootCall ) const {
  static size_t nodeVisitedCount;

  if (rootCall) {
    for (const auto &node : hashiPlay_->getTodoNodes())
      if (node.bridgesAbsent_ != 0  ||  node.bridgesWanted_ != node.bridgesFound_)
        return false;                            // not solved node found

    nodeVisitedCount = 0;
    resetFloodFlags();
  }

  nodeVisitedCount += 1;
  floodFlag_ = 1;                                // against closed paths (meshes in hashiwokakero)

  HashiNode::Neighbourhood others = {};
  HashiNode::getConnectedNodes( &others );       // look around and check connected neighbours too

  for (const auto &neighbour : others.allAround) // east, west, north, south
    if (neighbour.node != nullptr  &&  neighbour.node->floodFlag_ == 0)
      neighbour.node->hashiSolved( false );      // recursive depth-first search, meshes tolerant!

  if (rootCall)
    return (nodeVisitedCount == hashiPlay_->getTodoNodes().size());  // all nodes visited?
  else
    return false;                                // irrelevant return value
}  // -------------------------------------------------------------------------------------------


std::stack<HashiNode::Bridge> *HashiNode::getUndoStack( void ) { return sUndoStack_; }

void HashiNode::setUndoStack( std::stack<Bridge> *undoStack ) { sUndoStack_ = undoStack; }

void HashiNode::chargeBridgesOnUndoStack( std::stack<Bridge> *undoStack ) {
  // engage undo engine, makeBridge() will trace yourself
  //assert (hashiPlay_->_hashiMap.undoStack == nullptr);
  assert( getUndoStack() == nullptr );
  assert( undoStack != nullptr );
  assert( undoStack->size() == 0 );    // should be empty, at start of the undo mechanism

  setUndoStack( undoStack );
}  // -------------------------------------------------------------------------------------------

void HashiNode::dischargeBridgesFromUndoStack( void ) {
  // remove all bridges stored in undo stack and stop the bridge undo mechanism in makeBridge()
  std::stack<Bridge> *undoStack = getUndoStack();
  assert( undoStack != nullptr );

  setUndoStack( nullptr );                                 // disable tracing before undoBridge()

  //printf( "--- dischargeBridgeUndo() : %zu items to drop:\n", undoStack->size() );
  for (size_t n = undoStack->size(); n != 0; n--) {        // undo all steps stored on stack
    undoBridge( &undoStack->top() );
    undoStack->pop();
  }
}  // -------------------------------------------------------------------------------------------

bool HashiNode::deepAnalysis_errorConditionTest( void ) {
  // return true : if building temporary bridges with staticAnalysis() leads to any error condition
  // return false: if no more bridges can be made with staticAnalysis() AND there is _NO_ error condition
  // Produced bridges are cancelled(!) in both situations.

  std::stack<Bridge> undoStack;
  chargeBridgesOnUndoStack( &undoStack );        // make bridges only to see if it leads to error

  bool errorCondition = false;
  bool bridgesFound = {};
  do {                                           // repeat until solving gets stuck
    bridgesFound = false;
    for (auto &node : hashiPlay_->getTodoNodes()) {
      assert( node.bridgesAbsent_ == node.bridgesWanted_ - node.bridgesFound_ );
      assert( node.bridgesAbsent_ >= 0 );

      if (node.bridgesAbsent_ != 0  &&  node.staticAnalysis( &errorCondition ))
        bridgesFound = true;
      if (errorCondition)
        break;
    }
  } while (bridgesFound  &&  errorCondition == false);

  // no more bridges can be made by staticAnalysis(), hence look for an error condition:

  if (errorCondition) {                          // detected already by staticAnalysis() ?
    //printf( "--- --- --- --- : error condition detected by staticAnalysis() !!!\n" );
  } else
    errorCondition = anyNodeSeparated()  ||  isolatedGroup( true );

  dischargeBridgesFromUndoStack();               // cancel all experimentally made bridges

  return errorCondition;
}  // -------------------------------------------------------------------------------------------


bool HashiNode::ECA_2_NeighboursCase( HashiNode *node, Neighbour *others[], ECA_kit *kit ) {
  // return true, if _ALL_ possible bridge combinations between node/others[1]
  //              lead to an error condition, thus the single connection between 
  //              node and others[0] is _surely_ correct.
  // return false, if no connection can be guaranteed (or appropriate test is NOT implemented yet).

  assert( node->bridgesAbsent_ <= kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges );
  assert( kit->neighbourhood.neighbourCount == 2 );
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= (kit->neighbourhood.neighbourCount - 1) * 2 );

  const int neighbourCount  = kit->neighbourhood.neighbourCount;
  const int bridgesPossible = kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges;
  bool      errorCondition  = true;

  switch (int bridgesAbsent = node->bridgesAbsent_) {      // save before(!) temp connections
  case 1:  /* bridgesAbsent=1 + bridgesPossible = [1,2] */
    //            1 case:  (1 neighbour with at least single bridge potential)
    //
    // (X)--(n) ?(others[0])
    //

    if (kit->analysis->verbose >= 5)
      printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  1 neighbour with at least single bridge potential\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
    node->makeBridge( others[1], cSingleBridge );          // temporary connection
    errorCondition = (node->*kit->analysis->errorConditionTest)();
    node->dropBridge( others[1], cSingleBridge );          // get rid of temporary stuff
    break;
  case 2:  /* bridgesAbsent=2 + bridgesPossible = [2] */
    //            1 case:  (1 neighbour with at least single bridge potential)
    //
    // (X)==(n) ?(others[0])
    //

    if (kit->analysis->verbose >= 5)
      printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  1 neighbour with double bridge potential\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
    node->makeBridge( others[1], cDoubleBridge );          // temporary connection
    errorCondition = (node->*kit->analysis->errorConditionTest)();
    node->dropBridge( others[1], cDoubleBridge );          // get rid of temporary stuff
    break;
  }
  return errorCondition;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::ECA_3_NeighboursCase( HashiNode *node, Neighbour *others[], ECA_kit *kit ) {
  // return true, if _ALL_ possible bridge combinations between node/others[1,2]
  //              lead to any error condition, thus the single connection between 
  //              node and others[0] is _surely_ correct.
  // return false, if no connection can be guaranteed (or appropriate test is NOT implemented yet).

  assert( node->bridgesAbsent_ <= kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges );
  assert( kit->neighbourhood.neighbourCount == 3 );
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= (kit->neighbourhood.neighbourCount - 1) * 2 );

  const int neighbourCount  = kit->neighbourhood.neighbourCount;
  const int bridgesPossible = kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges;
  bool      errorCondition  = true;

  switch (int bridgesAbsent = node->bridgesAbsent_) {      // save before(!) temp connections
  case 1:  /* bridgesAbsent=1 + bridgesPossible = [2,3,4] */
    //            2 cases: (2 neighbours with at least single bridge potential)
    //      (X)                      (X)
    //       |
    // (X)  (n) ?(others[0])    (X)--(n) ?(others[0])
    //

    if (kit->analysis->verbose >= 4)
      printf( "%s_%dnc: try!! (A=%d P=%d) 2 cases, 2 neighbours with at least single bridge potential\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
    for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
      node->makeBridge( others[k], cSingleBridge );        // temporary
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[k], cSingleBridge );        // get rid of temporary
    }
    if (errorCondition  &&  kit->analysis->verbose >= 3)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              " THREE neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "2", "s" );
    break;
  case 2:  /* bridgesAbsent=2 + bridgesPossible = [2,3,4] */
    // if bridgesPossible == 2:  ---------------------------------------------
    //            1 case:  (2 neighbours with single bridge potential)
    //      (1)
    //       |
    // (1)--(n) ?(others[0])
    //
    // if bridgesPossible == 3:  ---------------------------------------------
    //            2 cases: (1 of 2 neighbours with double bridge but at 2 potential locations)
    //      (1)                      (1)
    //       |
    // (2)--(n) ?(others[0])    (2)==(n) ?(others[0])
    //
    // if bridgesPossible == 4:  ---------------------------------------------
    //            3 cases: (2 neighbours with double bridge potential)
    //      (2)                      (2)                      (2)
    //       |                                                 H
    // (2)--(n) ?(others[0])    (2)==(n) ?(others[0])    (2)  (n) ?(others[0])
    //

    if (kit->analysis->verbose >= 4)
      printf( "%s_%dnc: try!! (A=%d P=%d) %d case%s 2 neighbours\n",
              kit->analysis->name, neighbourCount,
              bridgesAbsent, bridgesPossible, bridgesPossible - 1, (bridgesPossible == 2) ? ", " : "s," );
    node->makeBridge( others[1], others[2], cSingleBridge );         // temp: 2x single bridges
    errorCondition = (node->*kit->analysis->errorConditionTest)();
    node->dropBridge( others[1], others[2], cSingleBridge );         // get rid of temporary

    for (int k = 1; k < neighbourCount  &&  errorCondition; k++)
      if (others[k]->maxNewBridges == cDoubleBridge) {
        node->makeBridge( others[k], cDoubleBridge );                // temp: 1x double bridge
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[k], cDoubleBridge );                // get rid of temporary
      }
    if (errorCondition  &&  kit->analysis->verbose >= 3)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              " THREE neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1..3", "s");
    break;
  case 3:  /* bridgesAbsent=3 + bridgesPossible = [3,4] */
    // if bridgesPossible == 3:  ---------------------------------------------
    //            1 case:  (1 of 2 neighbours with double bridge but at 2 potential locations)
    //      (1)
    //       |
    // (2)==(n) ?(others[0])
    //
    // if bridgesPossible == 4:  ---------------------------------------------
    //            2 cases: (2 neighbours with double bridge potential)
    //      (2)                      (2)
    //       |                        H
    // (2)==(n) ?(others[0])    (2)--(n) ?(others[0])
    //

    if (kit->analysis->verbose >= 4)
      printf( "%s_%dnc: try!! (A=%d P=%d) %d case%s 2 neighbours\n",
              kit->analysis->name, neighbourCount,
              bridgesAbsent, bridgesPossible, bridgesPossible - 2, (bridgesPossible == 3) ? ", " : "s," );
    node->makeBridge( others[1], others[2], cSingleBridge );         // temp: 2x single bridges

    for (int k = 1; k < neighbourCount  &&  errorCondition; k++)
      if (others[k]->maxNewBridges == cDoubleBridge) {
        node->makeBridge( others[k], cSingleBridge );                // temp: add single bridge
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[k], cSingleBridge );                // get rid of temp addition
      }
    node->dropBridge( others[1], others[2], cSingleBridge );         // get rid of temporary

    if (errorCondition  &&  kit->analysis->verbose >= 3)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              " THREE neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1..2", "" );
    break;
  case 4:  /* bridgesAbsent=4 + bridgesPossible = [4] */
    // if bridgesPossible == 4:  ---------------------------------------------
    //            1 case:  (2 neighbours with double bridge potential)
    //      (2)
    //       H
    // (2)==(n) ?(others[0])
    //

    if (kit->analysis->verbose >= 4)
      printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  2 neighbours\n", 
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
    node->makeBridge( others[1], others[2], cDoubleBridge );         // temp: 2x double bridges
    errorCondition = (node->*kit->analysis->errorConditionTest)();
    node->dropBridge( others[1], others[2], cDoubleBridge );         // get rid of temporary

    if (errorCondition  &&  kit->analysis->verbose >= 3)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              " THREE neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1", "" );
    break;
  }
  return errorCondition;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::ECA_4_NeighboursCase( HashiNode *node, Neighbour *others[], ECA_kit *kit ) {
  // return true, if _ALL_ possible bridge combinations between node/others[1,2,3]
  //              lead to any error condition, thus the single connection between 
  //              node and others[0] is _surely_ correct.
  // return false, if no connection can be guaranteed (or appropriate test is NOT implemented yet).

  assert( node->bridgesAbsent_ <= kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges );
  assert( kit->neighbourhood.neighbourCount == 4 );
  assert( node->bridgesAbsent_ > 0  &&  node->bridgesAbsent_ <= (kit->neighbourhood.neighbourCount - 1) * 2 );

  const int neighbourCount  = kit->neighbourhood.neighbourCount;
  const int bridgesPossible = kit->neighbourhood.bridgesPossible - others[0]->maxNewBridges;
  bool      errorCondition  = true;

  switch (int bridgesAbsent = node->bridgesAbsent_) {      // save before(!) temp connections
  case 1:  /* bridgesAbsent=1 + bridgesPossible = [3,4,5,6] */
    //            3 cases: (3 neighbours with at least single bridge potential)
    //      (X)                      (X)                      (X)
    //       |
    // (X)  (n) ?(others[0])    (X)--(n) ?(others[0])    (X)  (n) ?(others[0])
    //                                                         |
    //      (X)                      (X)                      (X)

    if (kit->analysis->verbose >= 3)
      printf( "%s_%dnc: try!! (A=%d P=%d) 3 cases, 3 neighbours with at least single bridge potential\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
    for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
      node->makeBridge( others[k], cSingleBridge );        // temporary: 1 single bridge
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[k], cSingleBridge );        // get rid of temporary stuff
    }
    if (errorCondition  &&  kit->analysis->verbose >= 1)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible , "3", "s" );
    break;

  case 2:  /* bridgesAbsent=2 + bridgesPossible = [3,4,5,6] */
    // bridgesPossible :: case 3:  -------------------------------------------
    //            3 cases: (all 3 neighbours with at least single bridge potential)
    //      (1)                      (1)                      (1)
    //       |
    // (1)  (n) ?(others[0])    (1)--(n) ?(others[0])    (1)  (n) ?(others[0])
    //                                                         |
    //      (1)                      (1)                      (1)
    // 
    // bridgesPossible :: case 4:  -------------------------------------------
    //            4 cases: (1 of 3 neighbours with double bridge, at 3 potential locations)
    //      (2)
    //       H
    // (1)  (n) ?(others[0])
    //
    //      (1)
    //
    //      (2)                      (2)                      (2)
    //       |                                                 |
    // (1)--(n) ?(others[0])    (1)--(n) ?(others[0])    (1)  (n) ?(others[0])
    //                                |                        |
    //      (1)                      (1)                      (1)
    // 
    // bridgesPossible :: case 5:  -------------------------------------------
    //            5 cases: (1 of 3 neighbours with single bridge, at 3 potential locations)
    //      (2)                      (2)
    //       H
    // (2)  (n) ?(others[0])    (2)==(n) ?(others[0])
    //
    //      (1)                      (1)
    //
    //      (2)                      (2)                      (2)
    //       |                                                 |
    // (2)--(n) ?(others[0])    (2)--(n) ?(others[0])    (2)  (n) ?(others[0])
    //                                |                        |
    //      (1)                      (1)                      (1)
    // 
    // bridgesPossible :: case 6:  -------------------------------------------
    //            6 cases: (all 3 neighbours with double bridge)
    //      (2)                      (2)                      (2)
    //       H
    // (2)  (n) ?(others[0])    (2)==(n) ?(others[0])    (2)  (n) ?(others[0])
    //                                                         H
    //      (2)                      (2)                      (2)
    //
    //      (2)                      (2)                      (2)
    //       |                                                 |
    // (2)--(n) ?(others[0])    (2)--(n) ?(others[0])    (2)  (n) ?(others[0])
    //                                |                        |
    //      (2)                      (2)                      (2)

    if (kit->analysis->verbose >= 3)
      printf( "%s_%dnc: try!! (A=%d P=%d) %d cases, 3 neighbours, but in all combinations\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, bridgesPossible );
    for (int k = 1; k < neighbourCount  &&  errorCondition; k++)
      if (others[k]->maxNewBridges == cDoubleBridge) {
        node->makeBridge( others[k], cDoubleBridge );      // temporary: 1 double bridge
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[k], cDoubleBridge );      // get rid of temporary stuff
      }
    for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
      int n = (k + 1 < neighbourCount) ? k + 1 : 1;        // "k+1" with wraparound
      node->makeBridge( others[k], others[n], cSingleBridge );       // temporary pair
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[n], others[k], cSingleBridge );       // get rid of temporary
    }
    if (errorCondition  &&  kit->analysis->verbose >= 1)
      printf( "%s_%dnc: sure case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
              "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
              kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "3..6", "s" );
    break;

  case 3:  // bridgesAbsent=3 + bridgesPossible = [3,4,5,6]
    switch (bridgesPossible) {
    case 4:
      //            3 cases: (1 of 3 neighbours have double bridge potential)
      //      (1)                      (1)                      (1)
      //       |                        |
      // (2)--(n) ?(others[0])    (2)==(n) ?(others[0])    (2)==(n) ?(others[0])
      //       |                                                 |
      //      (1)                      (1)                      (1)
    case 5:
      //            5 cases: (2 of 3 neighbours have double bridge potential)
      //      (2)                      (2)                      (2)
      //       |                        H                        H
      // (2)--(n) ?(others[0])    (2)--(n) ?(others[0])    (2)  (n) ?(others[0])
      //       |                                                 |
      //      (1)                      (1)                      (1)
      //
      //                               (2)                      (2)
      //                                                         |
      //                          (2)==(n) ?(others[0])    (2)==(n) ?(others[0])
      //                                |
      //                               (1)                      (1)
    case 6:
      //            7 cases: (all 3 neighbours have double bridge potential)
      //      (2)
      //       |
      // (2)--(n) ?(others[0])
      //       |
      //      (2)
      //
      //      (2)                      (2)                      (2)
      //       H                        H                        |
      // (2)--(n) ?(others[0])    (2)  (n) ?(others[0])    (2)==(n) ?(others[0])
      //                                |
      //      (2)                      (2)                      (2)
      //
      //      (2)                      (2)                      (2)
      //       |
      // (2)  (n) ?(others[0])    (2)--(n) ?(others[0])    (2)==(n) ?(others[0])
      //       H                        H                        |
      //      (2)                      (2)                      (2)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) %d cases, 3 single/double bridge neighbour collections\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, (bridgesPossible - 3) * 2 + 1 );

      for (int k = 1; k < neighbourCount  &&  errorCondition; k++)
        if (others[k]->maxNewBridges == cDoubleBridge) {
          node->makeBridge( others[k], cDoubleBridge );    // temporary: 1 double bridge

          for (int n = k, i = 2; i < neighbourCount  &&  errorCondition; i++) {
            n = (n + 1 < neighbourCount) ? n + 1 : 1;      // "k+1" with wraparound
            node->makeBridge( others[n], cSingleBridge );  // add temporary 1 single bridge
            errorCondition = (node->*kit->analysis->errorConditionTest)();
            node->dropBridge( others[n], cSingleBridge );  // get rid of single bridge
          }
          node->dropBridge( others[k], cDoubleBridge );    // get rid of temporary stuff
        }
      if (errorCondition == false)
        break;
      // !! intentionally fall into "case 3:", if errorCondition is true
    case 3:
      //            1 case:  (3 neighbours have single bridge potential)
      //      (1)
      //       |
      // (1)--(n) ?(others[0])
      //       |
      //      (1)

      if (kit->analysis->verbose >= 3  &&  bridgesPossible == 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  3 single bridge neighbours\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
      node->makeBridge( others[1], others[2], others[3], cSingleBridge );      // temporary
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[1], others[2], others[3], cSingleBridge );      // get rid of temporary

      if (errorCondition  &&  kit->analysis->verbose >= 1)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1..7", "s" );
      break;
    default:
      errorCondition = false;
      assert( 0 );
    }
    break;

  case 4:  // bridgesAbsent=4 + bridgesPossible = [4,5,6]
    switch (bridgesPossible) {
    case 4:
      //            1 case: (however: 1 double bridge neighbour has 3 potential locations)
      //      (2)                      (1)                      (1)
      //       H                        |                        |
      // (1)--(n) ?(others[0])    (2)==(n) ?(others[0])    (1)--(n) ?(others[0])
      //       |                        |                        H
      //      (1)                      (1)                      (2)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  1 double bridge neighbour has 3 potential locations\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );

      node->makeBridge( others[1], others[2], others[3], cMaxNewBridges );     // temporary connections
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[1], others[2], others[3], cMaxNewBridges );     // get rid of temp

      if (errorCondition  &&  kit->analysis->verbose >= 2)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1", "" );
      break;
    case 5:
      //            3 cases: (however: 1 single bridge neighbour has 3 potential locations)
      //      (1)                      (1)                      (1)
      //                                |                        |
      // (2)==(n) ?(others[0])    (2)==(n) ?(others[0])    (2)--(n) ?(others[0])
      //       H                        |                        H
      //      (2)                      (2)                      (2)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 3 cases, 1 single bridge neighbour has 3 potential locations\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
      for (int k = 1; k < neighbourCount; k++)                       // temp: both double bridges ONLY
        if (others[k]->maxNewBridges == cDoubleBridge)
          node->makeBridge( others[k], cDoubleBridge );

      errorCondition = (node->*kit->analysis->errorConditionTest)();

      for (int k = 1; k < neighbourCount; k++)                       // get rid of temporary stuff
        if (others[k]->maxNewBridges == cDoubleBridge)
          node->dropBridge( others[k], cDoubleBridge );
      if (errorCondition == false)
        break;

      node->makeBridge( others[1], others[2], others[3], cSingleBridge );      // temp: 3 single bridges

      for (int k = 1; k < neighbourCount  &&  errorCondition; k++)
        if (others[k]->maxNewBridges == cDoubleBridge) {
          node->makeBridge( others[k], cSingleBridge );
          errorCondition = (node->*kit->analysis->errorConditionTest)();
          node->dropBridge( others[k], cSingleBridge );
        }
      node->dropBridge( others[1], others[2], others[3], cSingleBridge );      // get rid of temp

      if (errorCondition  &&  kit->analysis->verbose >= 1)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "3", "s" );
      break;
    case 6:
      //            6 cases: (however: all neighbours have double bridge potential)
      //      (2)                      (2)                      (2)
      //       H                                                 H
      // (2)==(n) ?(others[0])    (2)==(n) ?(others[0])    (2)  (n) ?(others[0])
      //                                H                        H
      //      (2)                      (2)                      (2)
      //
      //      (2)                      (2)                      (2)
      //       H                        |                        |
      // (2)--(n) ?(others[0])    (2)==(n) ?(others[0])    (2)--(n) ?(others[0])
      //       |                        |                        H
      //      (2)                      (2)                      (2)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 6 cases, all neighbours have double bridge potential\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
      for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
        int n = (k + 1 < neighbourCount) ? k + 1 : 1;                // "k+1" with wraparound
        node->makeBridge( others[k], others[n], cDoubleBridge );     // temporary double bridge pair
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[n], others[k], cDoubleBridge );     // get rid of temporary pair
      }
      if (errorCondition == false)
        break;

      node->makeBridge( others[1], others[2], others[3], cSingleBridge );      // temp: 3 single bridges

      for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
        node->makeBridge( others[k], cSingleBridge );
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[k], cSingleBridge );
      }
      node->dropBridge( others[1], others[2], others[3], cSingleBridge );      // get rid of temporary

      if (errorCondition  &&  kit->analysis->verbose >= 1)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible), %s variant%s in error\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "6", "s" );
      break;
    default:
      errorCondition = false;
      assert( 0 );
    }
    break;

  case 5:  // bridgesAbsent=5 + bridgesPossible = [5,6]
    switch (bridgesPossible) {
    case 5:
      //            1 case: (however: 1 single bridge neighbour has 3 potential locations)
      //      (1)                      (2)                      (2)
      //       |                        H                        H
      // (2)==(n) ?(others[0])    (1)--(n) ?(others[0])    (2)==(n) ?(others[0])
      //       H                        H                        |
      //      (2)                      (2)                      (1)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  single bridge neighbour has 3 potential locations\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );

      node->makeBridge( others[1], others[2], others[3], cMaxNewBridges );     // temporary connections
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[1], others[2], others[3], cMaxNewBridges );     // get rid of temp

      if (errorCondition  &&  kit->analysis->verbose >= 2)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1", "" );
      break;
    case 6:
      //            3 cases: (all 3 neighbours have double bridge potential)
      //      (2)                      (2)                      (2)
      //       |                        H                        H
      // (2)==(n) ?(others[0])    (2)--(n) ?(others[0])    (2)==(n) ?(others[0])
      //       H                        H                        |
      //      (2)                      (2)                      (2)

      if (kit->analysis->verbose >= 3)
        printf ( "%s_%dnc: try!! (A=%d P=%d) 3 cases, all 3 neighbours have double bridge potential\n",
                 kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
      node->makeBridge( others[1], others[2], others[3], cSingleBridge );      // temporary

      for (int k = 1; k < neighbourCount  &&  errorCondition; k++) {
        int n = (k + 1 < neighbourCount) ? k + 1 : 1;                          // "k+1" with wraparound
        node->makeBridge( others[k], others[n], cSingleBridge );               // add 2 single bridges
        errorCondition = (node->*kit->analysis->errorConditionTest)();
        node->dropBridge( others[n], others[k], cSingleBridge );               // get rid of singles
      }
      node->dropBridge( others[1], others[2], others[3], cSingleBridge );      // get rid of temp

      if (errorCondition  &&  kit->analysis->verbose >= 1)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "3", "s" );
      break;
    default:
      errorCondition = false;
      assert( 0 );
    }
    break;

  case 6:  // bridgesAbsent=6 + bridgesPossible = [6]
    switch (bridgesPossible) {
    case 6:
      //             1 case: (all 3 neighbours have double bridge potential)
      //      (2)
      //       H
      // (2)==(n) ? (others[0])
      //       H
      //      (2)

      if (kit->analysis->verbose >= 3)
        printf( "%s_%dnc: try!! (A=%d P=%d) 1 case,  all 3 neighbours have double bridge potential\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible );
      node->makeBridge( others[1], others[2], others[3], cDoubleBridge );      // temp: 3x double bridge
      errorCondition = (node->*kit->analysis->errorConditionTest)();
      node->dropBridge( others[1], others[2], others[3], cDoubleBridge );      // get rid of temp

      if (errorCondition  &&  kit->analysis->verbose >= 2)
        printf( "%s_%dnc: case (errorCondition: FOUND)  !!! !!! !!! !!! !!! !!! !!! !!!\n"
                "  FOUR neighbours case (%d Absent, %d possible, %s variant%s in error)\n",
                kit->analysis->name, neighbourCount, bridgesAbsent, bridgesPossible, "1", "" );
      break;
    default:
      errorCondition = false;
      assert( 0 );
    }
    break;
  }
  return errorCondition;
}  // -------------------------------------------------------------------------------------------

bool HashiNode::errorConditionAnalysis( Analysis *analysis ) {
  // If node's temporarily connections built to all except one adjacent neighbours lead 
  // to an error condition, then (at least) a single bridge to the remaining unconnected 
  // neighbour can be surely build. However to guarantee this conclusion correctness 
  // _ALL_ possible combinations must lead to any of possible error conditions.
  // (Connections built temporarily have to be dropped in any case.)
  // 
  // Possible error conditions (after temporary connections building):
  // #1: any of nodes can't get required bridge amount due to the lack of free neighbours
  // #2: group of nodes becomes an isolated island (not necessarily at current node location)
  // #3: any of above errors appears after further ordinary solving steps (== deepAnalysis() method)
  // #4: unsolved nodes set is splited in separated groups, without a chance for connection
  //
  // Detection of conditions #1,#2 (advancedAnalysis) is made as one step (because of moderate overhead).
  // Detection of condition #3 (deepAnalysis) is performed separately due to its higher time overhead.
  // Detection of condition #4 is NOT implemented.

  ECA_kit kit = {};
  exploreNeighbourhood( &kit.neighbourhood );
  kit.analysis = analysis;

  assert( bridgesAbsent_ > 0 );
  assert( kit.neighbourhood.neighbourCount > 1  &&  kit.neighbourhood.bridgesPossible > bridgesAbsent_ );

  //printf( "%s:ORIGIN: (%u - %u = %d) @[%2zu,%-2zu] :: neighbourCnt: %d, possible: %d\n",
  //        analysis->name, _bridgesWanted, _bridgesFound, _bridgesAbsent, _row, _col,
  //        neighbourhood.neighbourCount, neighbourhood.bridgesPossible );

  std::array <Neighbour *, elementsof( kit.neighbourhood.allAround )> others = { nullptr };
  auto it = others.begin();                      // std::array<Neighbour *, 4>::iterator it = others.begin ();
  for (auto &other : kit.neighbourhood.allAround)
    if (other.maxNewBridges != 0)                // collect only active (unsolved) neighbours
      *it++ = &other;

  const int neighbourCount = kit.neighbourhood.neighbourCount;
  for (int i = 0; i < neighbourCount; i++) {
    if (i > 0)                                   // rotate active neighbourhood to analyse all directions
      std::rotate( others.begin(), others.begin() + 1, others.begin() + neighbourCount );

    assert( bridgesAbsent_ <= kit.neighbourhood.bridgesPossible - others[0]->maxNewBridges );
    assert( neighbourCount - 2 >= 0 &&
            neighbourCount - 2 < (int) elementsof( sECA_NeighboursSwitch ) );

    bool errorCondition = (*sECA_NeighboursSwitch[neighbourCount - 2])( this, others.data(), &kit);
    if  (errorCondition) {
      if (kit.analysis->verbose >= 2) {
        const char *countName[] = { "  TWO", "THREE", " FOUR" };
        printf( "%s: FOUND: [%2zu,%-2zu]-->[%2zu,%-2zu] / %s neighbours (A=%d P=%d) "
                "(node finishing in other direction(s) leads to error)\n",
                analysis->name,
                row_, col_, others[0]->node->row_, others[0]->node->col_,
                countName[neighbourCount - 2], bridgesAbsent_, kit.neighbourhood.bridgesPossible );
      }
      makeBridge( others[0], cSingleBridge );
      //hashiPlay_->printHashiField();
      return true;                               // found single bridge
    }
  }
  return false;
}  // -------------------------------------------------------------------------------------------


//bool HashiNode::BFS_Scan( int deep ) {
//  // deepAnalysis may be time consuming (recursion). Therefore deep analysis should
//  // be used AFTER staticAnalysis() and advacedAnalysis() methods, when they fail.
//  // Go through all nodes with few solving possibilities, which are potential seeds 
//  // for the deepAnalysis(). Start with each seed by "trying" to make bridges
//  // in alternate directions. Continue building further possible bridges (quantity
//  // limited to 'deep'-steps) which follows currently chosen direction.
//  // Also unconnected neighbours of chosen seed have to be continued with possible bridges
//  // (else perhaps some riddles cannot be solved(?) ).
//  // After each step check for nodes that become error condition (unsolved node becomes
//  // separated). In such case chosen path was wrong. Hence the opposite direction can
//  // be the correct solution! When all but one possibility lead to an error condition,
//  // then deepAnalysis() becomes successful.
//  // All bridges built during such "try" have to be dropped.
//  // Finally one single bridge can be surely build in this solitary direction, which
//  // was NOT in any error_condition. When more than one possible direction results 
//  // without error condition, then deep analysis fails.
//  // deepAnalysis test cases: Hashi level fiendish #38,33,34,36 and problematic #35!
//  //
//  // problematic test case for deepAnalysis(): Hashi level fiendish #37
//  //bool errorCondition = true;
//  assert( _bridgesAbsent != 0  &&  deep >= 0 );
//
//  //Neighbourhood neighbourhood = {};
//  //exploreNeighbourhood (&neighbourhood);
//  //assert (neighbourhood.neighbourCount > 1  &&  neighbourhood.bridgesPossible > _bridgesAbsent);
//  //printf ("DA:NODE: (%u - %u = %d) @[%2zu,%-2zu] :: neighbourCnt: %d, possible: %d\n",
//  //        bridgesWanted_, bridgesFound_, bridgesAbsent_, row_, col_,
//  //        neighbourhood.neighbourCount, neighbourhood.bridgesPossible);
//  //printf ("DA:NODE: (%u - %u = %d) @[%2zu,%-2zu]\n",
//  //        bridgesWanted_, bridgesFound_, bridgesAbsent_, row_, col_);
//
//  for (auto &node : hashiPlay_->getTodoNodes())
//    node.BFSvisitedFlag_ = 0;                              // clear in all nodes
//
//  BFSvisitedFlag_ = 1;
//  std::queue<HashiNode *> nodeQueue;                       // for Breadth First Search
//  nodeQueue.push( this );
//
//  while (nodeQueue.empty() == false) {
//    auto &node = nodeQueue.front();
//    printf( "DA:node: (%u - %u = %d) @[%2zu,%-2zu] :: from queue\n",
//            node->bridgesWanted_, node->bridgesFound_, node->bridgesAbsent_, node->row_, node->col_ );
//
//    Neighbourhood neighbourhood = {};
//    node->exploreNeighbourhood( &neighbourhood );
//    //problem ?? : exploreNeighbourhood() odcina mo¿liwoœæ po³¹czeñ typu 1-1 i 2=2.
//    //rozwi¹zanie: odcinanie po³¹czeñ typu 1-1 i 2=2 przesun¹æ(?) do osobnej funkcji, 
//    //             któr¹ wywo³ywaæ po u¿yciu exploreNeighbourhood() poza deepAnalysis()
//
//    if (deep > 0) {
//      if (node->bridgesAbsent_ > 0)
//        deep--;
//      for (auto &neighbour : neighbourhood.allAround)      // east, west, north, south
//        if (neighbour.node != nullptr && neighbour.node->BFSvisitedFlag_ == 0) {
//          neighbour.node->BFSvisitedFlag_ = 1;
//          nodeQueue.push( neighbour.node );
//        }
//    }
//    nodeQueue.pop();                                       // node reference becomes invalid
//  }
//  return false;
//}  // -------------------------------------------------------------------------------------------

HashiPlay::~HashiPlay () {
  delete[] outputField_;
  //delete[] inputField_;  -- currently not allocated at run time
}  // -------------------------------------------------------------------------------------------

HashiPlay::HashiPlay () :
  inputField_ { nullptr }, fieldRows_ { 0 }, fieldCols_ { 0 }, outputField_ { nullptr } {
  outFieldElem_ = 0;         // silence warning about uninitialized member variable
  printf( "HashiPlay() default constructor called! (!! Unexpected situation !!)\n" );
}  // -------------------------------------------------------------------------------------------

HashiPlay::HashiPlay (const uchar *inputField, size_t fieldRows, size_t fieldCols) :
  inputField_ { inputField }, fieldRows_ { fieldRows }, fieldCols_ { fieldCols } {
  outFieldElem_ = 0;         // silence warning about uninitialized member variable
#if 0
  // compiler/decltype trouble, hmm?
  outputField_ = new decltype (*outputField_)[fieldRows_ * fieldCols_];
#elif 0
  // compiler/decltype trouble, hmm?
  outputField_ = new decltype (outputField_[0])[fieldRows_ * fieldCols_];
#elif 1
  // decltype() works here together with new[]
  outputField_ = new decltype (outFieldElem_)[fieldRows_ * fieldCols_];
#else
  // clumsy way: works until codedElements type is changed...
  outputField_ = new uchar[fieldRows_ * fieldCols_];
#endif
}  // -------------------------------------------------------------------------------------------

//HashiPlay &HashiPlay::operator = ( const HashiPlay &rhs ) {
//  printf( "HashiPlay::operator= called\n" );
//  inputField_   = rhs.inputField_;               // shallow copy is ok
//  fieldRows_    = rhs.fieldRows_;
//  fieldCols_    = rhs.fieldCols_;
//  outputField_  = rhs.outputField_;              -- deep copy required!!
////outFieldElem_ = rhs.outFieldElem_;
//  todoNodes_    = rhs.todoNodes_;                -- deep copy required?
//  allNodes_     = rhs.allNodes_;                 -- deep copy required?
//
//  return *this;
//}  // -------------------------------------------------------------------------------------------

void HashiPlay::obtainAndCheckHashiData( void ) {
  assert( inputField_  != nullptr );
  assert( outputField_ != nullptr );

  size_t troubleCount = std::count_if( inputField_,
                                       inputField_ + fieldRows_ * fieldCols_,
                                       []( const auto elem ) { return HashiNode::isDataCorrupted( elem ); } );
  if (troubleCount != 0) {
    printf( "\nhashi Field: %zu data element%s corrupted.\n",
            troubleCount, (troubleCount == 1) ? " is" : "s are" );
    exit( 1 );
  }
  std::transform( inputField_,
                  inputField_ + fieldRows_ * fieldCols_, outputField_,
                  []( const auto elem ) { return (elem & cNodeBitMask); } );   // transfer islands only

}  // -------------------------------------------------------------------------------------------

//#define USE_EMPLACE_BACK_TO_AVOID_TWICE_CONSTRUCTOR_CALL
// problem: when _hashiMap.todoNodes.emplace_back() is used, then 
//          element's constructor _AND_ default destructor(!) are both called. Why...??
void HashiPlay::initHashiPlay( void ) {
  assert( inputField_  != nullptr );
  assert( outputField_ != nullptr );

  obtainAndCheckHashiData();

  size_t nodesCount = std::count_if( outputField_,
                                     outputField_ + fieldRows_ * fieldCols_,
                                     []( const auto elem ) { return elem != 0; } );

#if defined (USE_EMPLACE_BACK_TO_AVOID_TWICE_CONSTRUCTOR_CALL)
  todoNodes_.reserve( nodesCount );              // adjust required vector size at once
#else
  todoNodes_.resize( nodesCount );               // adjust required vector size at once
#endif
  allNodes_.resize( fieldRows_ * fieldCols_ );

  auto *node = todoNodes_.data();                // *node = &todoNodes[0];
  int i = 0;
  for (size_t row = 0; row < fieldRows_; row++)  // collect all nodes
    for (size_t col = 0; col < fieldCols_; col++, i++) {
      auto bridgesNeeded = outputField_[i];
      if (bridgesNeeded == 0)
        allNodes_[i] = nullptr;
      else {
      #if defined (USE_EMPLACE_BACK_TO_AVOID_TWICE_CONSTRUCTOR_CALL)
        todoNodes_.emplace_back( HashiNode( row, col, bridgesNeeded, this ) );
        allNodes_[i] = node++;              // node points to a valid object AFTER emplace_back()
      #else
        node->HashiNode::HashiNode( row, col, bridgesNeeded, this );
        allNodes_[i] = node++;
      #endif
      }
    }
}  // -------------------------------------------------------------------------------------------

void HashiPlay::printHashiField( const char *format, ... ) const {
  va_list args;
  va_start (args, format);
  vprintf (format, args);
  va_end (args);

  printHashiField();
}  // -------------------------------------------------------------------------------------------

void HashiPlay::printHashiField( void ) const {
  // todo! refactoring required!!
  printf( "   %c", 218 );                       // upper frame start
  for (int i = 0; i < fieldCols_; i++)
    printf( "%c", (i % 5) ? 196 : 193 );
  printf( "%c", 191 );
  printf( "       %c", 218 );                   // upper frame start
  for (int i = 0; i < fieldCols_; i++)
    printf( "%c", (i % 5) ? 196 : 193 );
  printf( "%c\n", 191 );

  int i = 0;
  for (size_t row = 0; row < fieldRows_; row++) {          // whole field
    bool mismatch = false;
    printf( "% 2zu:%c", row, 179 );
    for (size_t col = 0; col < fieldCols_; col++, i++) {
      uchar ch = outputField_[i];
      if (ch != inputField_[i])
        mismatch = true;

      switch (ch) {
      case 0x00: ch = ' '; break;
      #if 1
      case 0x50: ch = 196; break;     // single horizontal bridge
      case 0x60: ch = 205; break;     // double horizontal bridge
      case 0x90: ch = 179; break;     // single  VERTICAL  bridge
      case 0xa0: ch = 186; break;     // double  VERTICAL  bridge
      #else
      case 0x50: ch = '-'; break;     // single horizontal bridge
      case 0x60: ch = '='; break;     // double horizontal bridge
      case 0x90: ch = '|'; break;     // single  VERTICAL  bridge
      case 0xa0: ch = 'H'; break;     // double  VERTICAL  bridge
      #endif
      default:   ch = (ch <= 8) ? ch + '0' : '?';
      }
      printf( "%c", ch );
    }
    printf( "%c", 179 );

    i -= (int) fieldCols_;
    printf( " %s % 2zu:%c", (mismatch) ? "!=" : "  ", row, 179 );
    for (size_t col = 0; col < fieldCols_; col++, i++) {
      uchar ch = inputField_[i];
      switch (ch) {
      case 0x00: ch = ' '; break;
      #if 1
      case 0x50: ch = 196; break;     // single horizontal bridge
      case 0x60: ch = 205; break;     // double horizontal bridge
      case 0x90: ch = 179; break;     // single  VERTICAL  bridge
      case 0xa0: ch = 186; break;     // double  VERTICAL  bridge
      #else
      case 0x50: ch = '-'; break;     // single horizontal bridge
      case 0x60: ch = '='; break;     // double horizontal bridge
      case 0x90: ch = '|'; break;     // single  VERTICAL  bridge
      case 0xa0: ch = 'H'; break;     // double  VERTICAL  bridge
      #endif
      default:   ch = (ch <= 8) ? ch + '0' : '?';
      }
      printf( "%c", ch );
    }
    printf( "%c\n", 179 );
  }

  printf( "   %c", 192 );
  for (int i = 0; i < fieldCols_; i++)
    printf( "%c", (i % 5) ? 196 : 194 );
  printf( "%c", 217 );                 // finish bottom frame
  printf( "       %c", 192 );
  for (int i = 0; i < fieldCols_; i++)
    printf( "%c", (i % 5) ? 196 : 194 );
  printf( "%c\n", 217 );               // finish bottom frame
}  // -------------------------------------------------------------------------------------------

bool HashiPlay::hashiSolved( void ) const {

  if (todoNodes_.size() == 0)
    return true;
  else
    return todoNodes_[0].hashiSolved( true );    // recursive HashiNode::method, may start at any node
}  // -------------------------------------------------------------------------------------------

void HashiPlay::hashiSolvingCheck( HashiNode::Analysis analyses[], size_t analysesCount ) const {
  int differenceCount      = 0;
  int noBridgeCount        = 0;
  int narrowBridgeCount    = 0;
  int weirdDifferenceCount = 0;

  int i = 0;
  for (size_t row = 0; row < fieldRows_; row++) {          // whole field
    for (size_t col = 0; col < fieldCols_; col++, i++) {
      uchar referenceCode = inputField_[i];
      uchar solvedCode    = outputField_[i];

      if (referenceCode != solvedCode) {
        differenceCount++;
        if ((referenceCode & 0xF0) != 0 && (solvedCode & 0xF0) == 0)          // todo! better constants names or use filter functions
          noBridgeCount++;
        else if ((referenceCode & 0x20) != 0 && (solvedCode & 0x10) == 0x10)  // single instead of double bridge
          narrowBridgeCount++;
        else
          weirdDifferenceCount++;
      }
    }
  }
  printf( "\n+++ hashi internal check: %s +++ \n", hashiSolved() ? "Passed !" : " >>> FAILED <<<" );
  printf( "+++ hashi compare  check: %s +++", (differenceCount == 0) ? "Successful" : "FAILED" );
  for (size_t s = 0; s < analysesCount; s++) {
    if (s == 0)
      printf( " (" );
    printf( "%s: %zu hit%s", analyses[s].name, analyses[s].count, (analyses[s].count == 1) ? "" : "s" );
    printf( (s < analysesCount - 1) ? ", " : ")\n" );
  }
  if (differenceCount != 0) {
    printf( "--- hashi solving compare difference%s: %d", (differenceCount == 1) ?  "" : "s", differenceCount );
    if (noBridgeCount)
      printf( ", no bridge%s: %d", (noBridgeCount == 1) ?  "" : "s", noBridgeCount );
    if (narrowBridgeCount)
      printf( ", narrow bridge%s: %d", (narrowBridgeCount == 1) ? "" : "s", narrowBridgeCount );
    if (weirdDifferenceCount)
      printf( ", weird difference%s: %d\n", (weirdDifferenceCount == 1) ? "" : "s", weirdDifferenceCount );
    printf( "\n" );
  }
}  // -------------------------------------------------------------------------------------------

void HashiPlay::hashiSolve( int riddleId ) {
  HashiNode::Analysis analyses[] = {
    {&HashiNode::advancedAnalysis_errorConditionTest, "AA ", 0, /* verbose level: */ 0},
    {&HashiNode::deepAnalysis_errorConditionTest,     " DA", 0, /* verbose level: */ 0},
  };

  printHashiField( "hashiSolve() start  (size: %zu x %zu, riddleId: %d)\n",
                   fieldCols_, fieldRows_, riddleId );

  for (bool bridgesFound = true; bridgesFound; ) {
    bridgesFound = false;
    size_t nodeSolvedCount = 0;
    for (auto &node : todoNodes_) {
      bool errorCondition = false;
      assert( node.bridgesAbsent_ == node.bridgesWanted_ - node.bridgesFound_ );
      assert( node.bridgesAbsent_ >= 0 );

      if (node.bridgesAbsent_ == 0)
        nodeSolvedCount += 1;
      else if (node.staticAnalysis( &errorCondition )) {
        nodeSolvedCount += 1;
        bridgesFound = true;
      }
      if (errorCondition) {
        printf( "HashiField corrupted aroud NODE: (%u - %u = %d) @[%2zu,%-2zu])\n",
                node.bridgesWanted_, node.bridgesFound_, node.bridgesAbsent_, node.row_, node.col_ );
        bridgesFound = false;
        break;
      }
    }
    if (nodeSolvedCount == todoNodes_.size())
      break;                           // whole hashiwokakero riddle solved!

    if (bridgesFound == false)
      for (auto &analysis : analyses) {
        //printf( "Trying %s analysis...\n", analysis.name);
        //printHashiField ("Trying %s analysis...\n", analysis.name);
        for (auto &node : todoNodes_)
          if (node.bridgesAbsent_ != 0  &&  node.errorConditionAnalysis( &analysis )) {
            bridgesFound = true;       // stop expensive analysis
            break;                     // and fall back to STATIC analysis
          }
        if (bridgesFound) {
          analysis.count += 1;
          break;                       // skip next analysis, because previous was successful
        }
      }
  }
  printHashiField();
  hashiSolvingCheck( analyses, elementsof (analyses));
}  // -------------------------------------------------------------------------------------------

