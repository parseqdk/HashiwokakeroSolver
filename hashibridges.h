//
// Hashiwokakero Solver - engine
// by d/x, Spring 2022, Daniel Koziarski
//

//#pragma once
//#pragma pack(1)  // disables padding structures with hidden bytes for 32/64 bit alignment

#ifndef _HASHIBRIDGES_H
#define _HASHIBRIDGES_H

//#define NDEBUG

#include <cassert>
#include <cstddef>
#include <cstdio>
#include <vector>
#include <stack>
#include <algorithm>

#if 0
template<typename T>
constexpr auto elementsof(T x) { return sizeof (x) / sizeof ((x)[0]); }
template<typename T>
constexpr auto capacityof(T x) { return static_cast<size_t>( (&(x))[1] - (x) ); }
#else
#define elementsof(x)  (sizeof (x) / sizeof ((x)[0]))      // C6384 when used with pointer declared like ...(*ptr)[N];
#define capacityof(x)  (static_cast<size_t>( (&(x))[1] - (x) ))
#endif

#define DOUBLE_FRAME_UL_CORNER         201
#define DOUBLE_FRAME_UR_CORNER         187
#define DOUBLE_FRAME_LL_CORNER         200
#define DOUBLE_FRAME_LR_CORNER         188
#define DOUBLE_FRAME_HORIZONTAL_BAR    205
#define DOUBLE_FRAME_VERTICAL_BAR      186
#define DOUBLE_FRAME_LEFT_JUNCTION     204
#define DOUBLE_FRAME_RIGHT_JUNCTION    185
#define DOUBLE_FRAME_TOP_JUNCTION      203
#define DOUBLE_FRAME_BOTTOM_JUNCTION   202
#define DOUBLE_FRAME_CROSS             206

#define SINGLE_FRAME_UL_CORNER         218
#define SINGLE_FRAME_UR_CORNER         191
#define SINGLE_FRAME_LL_CORNER         192
#define SINGLE_FRAME_LR_CORNER         217
#define SINGLE_FRAME_HORIZONTAL_BAR    196
#define SINGLE_FRAME_VERTICAL_BAR      179
#define SINGLE_FRAME_LEFT_JUNCTION     195
#define SINGLE_FRAME_RIGHT_JUNCTION    180
#define SINGLE_FRAME_TOP_JUNCTION      194
#define SINGLE_FRAME_BOTTOM_JUNCTION   193
#define SINGLE_FRAME_CROSS             197

typedef unsigned char uchar;

constexpr signed char cSingleBridge  = 1;
constexpr signed char cDoubleBridge  = 2;
constexpr signed char cMaxNewBridges = 3;

constexpr uchar cSingleBridgeBitMask     = 0x10;
constexpr uchar cDoubleBridgeBitMask     = 0x20;
constexpr uchar cHorizontalBridgeBitMask = 0x40;
constexpr uchar cVerticalBridgeBitMask   = 0x80;

constexpr uchar cNodeBitMask             = 0x0F;
constexpr uchar cBridgeBitMask           = 0xF0;
constexpr uchar cBridgeDirectionBitMask  = cHorizontalBridgeBitMask | cVerticalBridgeBitMask;
constexpr uchar cBridgeWidthBitMask      = cSingleBridgeBitMask | cDoubleBridgeBitMask;

constexpr uchar cNoBridgeCode               = 0;
constexpr uchar cSingleHorizontalBridgeCode = cHorizontalBridgeBitMask | cSingleBridgeBitMask;
constexpr uchar cDoubleHorizontalBridgeCode = cHorizontalBridgeBitMask | cDoubleBridgeBitMask;
constexpr uchar cSingleVerticalBridgeCode   = cVerticalBridgeBitMask   | cSingleBridgeBitMask;
constexpr uchar cDoubleVerticalBridgeCode   = cVerticalBridgeBitMask   | cDoubleBridgeBitMask;


class HashiPlay;

class HashiNode {
  class Bridge {
    HashiNode *node_, *other_;
    char       bridgeType_;
  public:
    Bridge( HashiNode *node, HashiNode *other, char bridgeType );

    friend class HashiNode;
    // todo! dlaczego nie mo¿na zaprzyjaŸniæ konkretnych funkcji z klasy HashiNode,
    //       jeœli nie s¹ one publiczne, ale mo¿na zaprzyjaŸniæ ca³¹ klasê HashiNode 
    //       i wówczas mieæ dostêp do prywatnych metod z HashiNode ?
    //friend void HashiNode::undoBridge (const Bridge *bridge); //-- works ONLY, if undoBridge() is public member or class Bridge is friend of HashiNode
  };

  typedef struct {
    HashiNode *node;
    int        bridgeWidth;            // 0 = none, 1 = single, 2 = double
    int        bridgeLength;
    int        maxNewBridges;          // 0, 1 or 2
  } Neighbour;

  typedef struct {
    union {
      struct {
        Neighbour  east, west, north, south;
      };
      Neighbour  allAround[4];         // loop access
    };
    int        neighbourCount;         // neighbours quantity with currently available connection
    int        bridgesPossible;        // total in all possible directions
  } Neighbourhood;

  typedef struct {
    bool      (HashiNode::*errorConditionTest)( void );
    const char *name;        // for protocol purpose
    size_t     count;        // final statistic
    char       verbose;      // amount of console output protocol
  } Analysis;

  size_t row_, col_;         // node coordinates in hashi field of coded elements
  int    bridgesWanted_;     // bridges count required to solve current node
  int    bridgesFound_;      // already found bridges count
  int    bridgesAbsent_;     // bridges count remaining to find

  mutable
  char   floodFlag_;         // against circular/endless recursion (meshes possible!) in isolatedGroup()

  //char   BFSvisitedFlag_;  -- finally not used by deepAnalysis()

  HashiPlay *const hashiPlay_;  // const qualifier implicitly deletes the HashiNode::operator= !!!

  static std::stack<Bridge> *sUndoStack_;

  static int sObjCounter_;

  friend class HashiPlay;
  // Is it possible to make only a selected method from another class to be a friend of own class??
  //friend HashiPlay::hashiSolve(void); 
public:
  HashiNode();
  ~HashiNode();

private:
  HashiNode *initHashiNode( size_t row, size_t col, int bridgesNeeded, HashiPlay *hashiPlay );
  HashiNode( size_t row, size_t col, int bridgesNeeded, HashiPlay *hashiPlay );

  inline uchar *getHashiFieldLocation( void ) const;
  inline size_t getHashiFieldCols    ( void ) const;
  inline size_t getHashiFieldRows    ( void ) const;

  static inline int getHashiMapEastwardStep ( void );
  static inline int getHashiMapWestwardStep ( void );
         inline int getHashiMapNorthwardStep( void ) const;
         inline int getHashiMapSouthwardStep( void ) const;

  static void initNeighbour( Neighbour *neighbour );
  static void initNeighbour( Neighbour *neighbour, const HashiNode *node, int bridgeWidth, int bridgeLength );
  static inline int maxNewBridges( const Neighbour *neighbour ) {
    const int bridgesAbsent = neighbour->node->bridgesAbsent_;

    switch (neighbour->bridgeWidth) {
    case 0:
      return (bridgesAbsent < 2) ? bridgesAbsent : 2;
    case 1:
      return (bridgesAbsent > 0) ? 1 : 0;
    default:
      return 0;
    }
  }  // -------------------------------------------------------------------------------------------
#if 0
  static inline bool isEmpty ( const uchar hashiFieldElement ) { return (hashiFieldElement) == 0; }
  static inline bool isBridge( const uchar hashiFieldElement ) { return (hashiFieldElement & 0x30) != 0; }
  static inline bool isNode  ( const uchar hashiFieldElement ) { return (hashiFieldElement & 0x0F) != 0; }

  static inline bool is_Any_HorizontalBridge ( const uchar hfElement ) { return (hfElement & 0x4F) == 0x40; }
  static inline bool is_Any_VerticalBridge   ( const uchar hfElement ) { return (hfElement & 0x8F) == 0x80; }
  static inline bool isSingleHorizontalBridge( const uchar hfElement ) { return (hfElement) == 0x50; }
  static inline bool isSingleVerticalBridge  ( const uchar hfElement ) { return (hfElement) == 0x90; }
  static inline bool isDoubleHorizontalBridge( const uchar hfElement ) { return (hfElement) == 0x60; }
  static inline bool isDoubleVerticalBridge  ( const uchar hfElement ) { return (hfElement) == 0xa0; }
  static inline bool isSingleBridge          ( const uchar hfElement ) { return (hfElement & 0x3F) == 0x10; }
  static inline bool isDoubleBridge          ( const uchar hfElement ) { return (hfElement & 0x3F) == 0x20; }

  static inline int  getBridgeWidth          ( const uchar hfElement ) { return (hfElement & 0x30) >> 4; };
#else
  static inline bool isEmpty ( const uchar el ) { return el == 0; }
  static inline bool isBridge( const uchar el ) { return (el & cBridgeWidthBitMask) != 0; }
  static inline bool isNode  ( const uchar el ) { return (el & cNodeBitMask) != 0; }

  static inline bool is_Any_HorizontalBridge ( const uchar el ) {
    return (el & (cHorizontalBridgeBitMask | cNodeBitMask)) == cHorizontalBridgeBitMask;
  }
  static inline bool is_Any_VerticalBridge   ( const uchar el ) {
    return (el & (cVerticalBridgeBitMask   | cNodeBitMask)) == cVerticalBridgeBitMask;
  }
  static inline bool isSingleHorizontalBridge( const uchar el ) { return el == cSingleHorizontalBridgeCode; }
  static inline bool isSingleVerticalBridge  ( const uchar el ) { return el == cSingleVerticalBridgeCode; }
  static inline bool isDoubleHorizontalBridge( const uchar el ) { return el == cDoubleHorizontalBridgeCode; }
  static inline bool isDoubleVerticalBridge  ( const uchar el ) { return el == cDoubleVerticalBridgeCode; }
  static inline bool isSingleBridge          ( const uchar el ) {
    return (el & (cBridgeWidthBitMask | cNodeBitMask)) == cSingleBridgeBitMask; 
  }
  static inline bool isDoubleBridge          ( const uchar el ) {
    return (el & (cBridgeWidthBitMask | cNodeBitMask)) == cDoubleBridgeBitMask;
  }
  static inline bool isDataCorrupted( const uchar el ) {

    if ((el & cNodeBitMask) > 8)                                               // illegal node value?
      return true;
    if ((el & cNodeBitMask) != 0 && (el & cBridgeBitMask) != 0)                // node AND bridge in tandem?
      return true;
    if ((el & cBridgeDirectionBitMask) == cBridgeDirectionBitMask  ||          // both bridge directions set?
        (el & cBridgeWidthBitMask)     == cBridgeWidthBitMask)                 // both bridge width bits set?
      return true;
    if ((el & cBridgeDirectionBitMask) != 0 && (el & cBridgeBitMask) == 0  ||  // direction set, but with == 0?
        (el & cBridgeDirectionBitMask) == 0 && (el & cBridgeBitMask) != 0)     // no direction, but width != 0?
     return true;

    return false;            // no trouble in data coding detected
  }
  static inline int  getBridgeWidth          ( const uchar el ) { return (el & cBridgeWidthBitMask) >> 4; };
#endif

  void getEastNeighbour ( Neighbour *neighbour ) const;
  void getWestNeighbour ( Neighbour *neighbour ) const;
  void getNorthNeighbour( Neighbour *neighbour ) const;
  void getSouthNeighbour( Neighbour *neighbour ) const;
  
  void exploreNeighbourhood( Neighbourhood *others ) const;
  void getConnectedNodes   ( Neighbourhood *others ) const;

  void protocolBridgesMadeOnNodes(Neighbour *other, int bridgesMade );
  void makeHorizontalBridge( Neighbour *neighbour,                        char bridgeType );
  void makeVerticalBridge  ( Neighbour *neighbour,                        char bridgeType );
  void makeBridge          ( Neighbour *neighbour,                        char bridgeType );
  void makeBridge          ( Neighbour *n1, Neighbour *n2,                char bridgeType );
  void makeBridge          ( Neighbour *n1, Neighbour *n2, Neighbour *n3, char bridgeType );
  void dropBridge          ( Neighbour *neighbour,                        char bridgeType );
  void dropBridge          ( Neighbour *n1, Neighbour *n2,                char bridgeType );
  void dropBridge          ( Neighbour *n1, Neighbour *n2, Neighbour *n3, char bridgeType );

  static void undoBridge   ( const Bridge *bridge );

  bool makeSingleBridges    ( Neighbourhood *others );
  bool makeDoubleBridges    ( Neighbourhood *others );
  bool makeSureSingleBridges( Neighbourhood *others );
  bool make_All_Bridges     ( Neighbourhood *others );

  static bool SA_1_NeighbourCase        ( HashiNode *node, Neighbourhood *others );
  static bool SA_2_NeighboursCase       ( HashiNode *node, Neighbourhood *others );
  static bool SA_3_NeighboursCase       ( HashiNode *node, Neighbourhood *others );
  static bool SA_4_NeighboursCase       ( HashiNode *node, Neighbourhood *others );
  static bool (*sSA_NeighboursSwitch[4])( HashiNode *node, Neighbourhood *others );

  bool staticAnalysis( bool *const errorCondition );

  int  countBridgesPossibleAround( void ) const;
  bool anyNodeSeparated   ( void ) const;
  void resetFloodFlags    ( void ) const;
  bool isolatedGroup             ( bool resetFloodFlagsRequired ) const;
  bool advancedAnalysis_errorConditionTest( void );

  bool hashiSolved( bool rootCall ) const;

  static std::stack<Bridge> *getUndoStack( void );
  static void setUndoStack( std::stack<Bridge> *undoStack );
  void chargeBridgesOnUndoStack( std::stack<Bridge> *undoStack );
  void dischargeBridgesFromUndoStack( void );
  bool deepAnalysis_errorConditionTest( void );

  typedef struct {
    Neighbourhood neighbourhood;
    Analysis     *analysis;
  } ECA_kit;       // ErrorConditionAnalysis toolkit
  static bool ECA_2_NeighboursCase       ( HashiNode *node, Neighbour *others[], ECA_kit *kit );
  static bool ECA_3_NeighboursCase       ( HashiNode *node, Neighbour *others[], ECA_kit *kit );
  static bool ECA_4_NeighboursCase       ( HashiNode *node, Neighbour *others[], ECA_kit *kit );
  static bool (*sECA_NeighboursSwitch[3])( HashiNode *node, Neighbour *others[], ECA_kit *kit );

  bool errorConditionAnalysis( Analysis *analysis );

  //bool BFS_Scan( int deep );   -- finally not used
};

class HashiPlay {                // Hashiwokakero riddle playground
  const uchar *inputField_;      // input data (islands, optionally with riddle's solving bridges)

  size_t       fieldRows_,       // vertical   hashi field size
               fieldCols_;       // horizontal hashi field size

  uchar       *outputField_,     // output data, initialized with nodes obtained from input vector
               outFieldElem_;    // for "new + decltype()" workaround in HashiPlay() parametrized constructor

  std::vector<HashiNode>  todoNodes_;  // obtained nodes, i.e. islands to be connected together
  std::vector<HashiNode *> allNodes_;  // rapid access via "row+col"-offset (same size as _outputField)

public:
  ~HashiPlay();
  HashiPlay() /* = delete */;
  HashiPlay( const uchar *hashiField, size_t fieldRows, size_t fieldCols );
  // block copy constructor (HashiPlay hp = ...;), else deep copy implementation required:
  HashiPlay( const HashiPlay &rhs ) = delete;               // copy constructor: blocked

  // block assignment operator (HashiPlay hp1, hp2;  hp1 = hp2;), deep copy NOT implemented
  HashiPlay &operator = ( const HashiPlay &rhs ) = delete;  // copy assignment: blocked
  HashiPlay &operator = (       HashiPlay &&rhs ) = delete; // move assignment: blocked

  void initHashiPlay  ( void );
  void printHashiField( void ) const;
  void printHashiField( const char *title, ... ) const;
  void hashiSolve     ( int riddleId = -1 );

  inline size_t getHashiFieldRows  ( void ) const { return fieldRows_; }
  inline size_t getHashiFieldCols  ( void ) const { return fieldCols_; }
  inline uchar *getHashiOutputField( void ) const { return outputField_; }
  inline std::vector<HashiNode *> &getAllNodes ( void ) { return allNodes_; }                    
  inline std::vector<HashiNode>   &getTodoNodes( void ) { return todoNodes_; }

private:
  void obtainAndCheckHashiData( void );
  bool hashiSolved( void ) const;
  void hashiSolvingCheck( HashiNode::Analysis analyses[], size_t analysesCount ) const;
};

#endif  // !_HASHIBRIDGES_H
