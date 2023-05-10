#ifndef AST_H
#define AST_H

#include <llvm/IR/Value.h>
#include <algorithm>
#include <cassert>
#include <iostream>
#include <list>
#include <locale>
#include <map>
#include <memory>
#include <string>
#include <type_traits>
#include <typeinfo>
#include <utility>
#include <vector>

namespace spc {
// 顶层类
struct AbstractNode;
struct DummyNode;
struct ExprNode;
struct LeftValueExprNode;
struct StmtNode;
struct IdentifierNode;
// type def
struct TypeNode;
struct SimpleTypeNode;
struct StringTypeNode;
struct AliasTypeNode;
struct ArrayTypeNode;
struct ConstValueNode;
// 字面量相关
struct StringNode;
struct RealNode;
struct IntegerNode;
struct CharNode;
struct BoolenNode;
// 表达式相关
struct BinopExprNode;
struct FuncExprNode;
struct SysRoutineNode;
struct SysCallNode;
struct ArgListNode;
struct ParamDeclNode;
struct ParamListNode;
struct VarDeclNode;
struct VarListNode;
struct ConstDeclNode;
struct ConstListNode;
struct TypeDefNode;
struct TypeListNode;
// 结构语句相关
struct NameListNode;
struct RoutineCallNode;
struct SubroutineListNode;
struct HeadListNode;
struct RoutineNode;
struct ProgramNode;
struct SubroutineNode;
struct CompoundStmtNode;
struct AssignStmtNode;
struct ProcStmtNode;
struct StmtList;

struct CodegenContext;
/**
 * @brief 判断指针是否是某个节点类型的子类指针
 * @tparam NodeType 节点类型
 * @param ptr 要判断的指针
 * @return true 如果指针是NodeType的子类指针
 * @return false 如果指针不是NodeType的子类指针
 */
template <typename NodeType>
bool is_a_ptr_of(const std::shared_ptr<AbstractNode> &ptr) {
  auto _p = ptr.get();
  return dynamic_cast<NodeType *>(_p) != nullptr;
}


/**
 * @brief 用裸指针创建一个智能指针
 * @param node 要包装的裸指针
 * @return std::shared_ptr<AbstractNode> 所创建的智能指针
 */
inline std::shared_ptr<AbstractNode> wrap_node(AbstractNode *node) { return std::shared_ptr<AbstractNode>{node}; }

/**
 * @brief 将智能指针转换为指定类型的智能指针
 * @tparam TNode 节点类型
 * @param node 要转换的智能指针
 * @return std::enable_if<std::is_base_of<AbstractNode, TNode>::value, std::shared_ptr<TNode>>::type 转换后的指针，如果转换失败则返回nullptr
 */
template <typename TNode>
typename std::enable_if<std::is_base_of<AbstractNode, TNode>::value, std::shared_ptr<TNode>>::type cast_node(
    const std::shared_ptr<AbstractNode> &node) {
  if (is_a_ptr_of<TNode>(node)) return std::dynamic_pointer_cast<TNode>(node);
  assert(is_a_ptr_of<TNode>(node));
  return nullptr;
}

/**
 * @brief 
 * @tparam NodeType 
 * @tparam Args 
 * @param args 
 * @return std::shared_ptr<AbstractNode> 
 */
template <typename NodeType, typename... Args>
std::shared_ptr<AbstractNode> make_node(Args &&... args) {
  return std::dynamic_pointer_cast<AbstractNode>(std::make_shared<NodeType>(std::forward<Args>(args)...));
};

/**
 * @brief AST节点抽象类
 */
struct AbstractNode : public std::enable_shared_from_this<AbstractNode> {
  std::list<std::shared_ptr<AbstractNode>> _children;///< 子节点列表
  std::weak_ptr<AbstractNode> _parent;///< 父节点指针

  /**
   * @brief 抽象节点对象的析构函数
   */
  virtual ~AbstractNode() noexcept = default;
  
  /**
   * @brief 代码生成方法，需要由子类实现
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的代码值
   */
  virtual llvm::Value *codegen(CodegenContext &context) = 0;
  /**
   * @brief 打印Json字符串
   */
  void print_json() const;
   /**
   * @brief 将节点转为Json字符串
   * @return std::string 转化后的Json字符串
   */
  std::string to_json() const;
  
   /**
   * @brief 获取子节点列表
   * @return std::list<std::shared_ptr<AbstractNode>>& 子节点列表
   */
  std::list<std::shared_ptr<AbstractNode>> &children() noexcept {
    assert(this->should_have_children());
    return this->_children;
  }
  
    /**
   * @brief 获取父节点指针
   * @return auto& 父节点指针
   */
  auto &parent() noexcept { return this->_parent; }
  
  /**
   * @brief 添加子节点
   * @param node 子节点,使用左值引用参数
   */
  virtual void add_child(const std::shared_ptr<AbstractNode> &node) {
    this->_children.push_back(node);
    node->parent() = this->shared_from_this();
  }

  /**
   * @brief 添加子节点
   * @param node 子节点，使用C++11的新特性右值引用作为参数，避免构造函数多次调用，节约时间
   */
  virtual void add_child(std::shared_ptr<AbstractNode> &&node) {
    this->_children.push_back(node);
    node->parent() = this->shared_from_this();
  }

  /**
   * @brief 合并子节点列表
   * @param children 子节点列表
   */
  void merge_children(const std::list<std::shared_ptr<AbstractNode>> &children) {
    for (const auto &e : children) {
      this->add_child(e);
    }
  }

  /**
   * @brief 提取子节点并添加到当前节点
   * @param node 被提取的节点
   */
  void lift_children(const std::shared_ptr<AbstractNode> &node) { this->merge_children(node->children()); }
  
  /**
   * @brief 是否应该有子节点
   * @return true 表示应该有子节点
   * @return false 表示不应该有子节点
   */
  virtual bool should_have_children() const { return true; }
  
  /**
   * @brief 节点头部信息，需要由子类实现
   * @return std::string 头部信息字符串
   */
  virtual std::string json_head() const = 0;
};
/**
 * @brief 抽象语法树节点类,是 AbstractNode 类的实现类，表示一些没有特定语义的节点
 */
struct DummyNode : public AbstractNode {
 public:
 /**
  * @brief 获取节点头部信息
  * @return std::string 头部信息字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"<unspecified-from-dummy>\""}; }

/**
 * @brief 生成IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的IR代码
 */
  llvm::Value *codegen(CodegenContext &context) override {
    std::cout << typeid(*this).name() << std::endl;// 输出当前对象的类型
    assert(false);// 在运行时发生错误时终止程序
    return nullptr;
  }
};

/**
 * @brief 定义表达式节点类
 */
struct ExprNode : public DummyNode {
  std::shared_ptr<TypeNode> type;

 protected:
  ExprNode() = default;
};
/**
 * @brief 左值表达式节点类
 */
struct LeftValueExprNode : public ExprNode {
  virtual llvm::Value *get_ptr(CodegenContext &context) = 0;
  std::string name;
};
/**
 * @brief 语句节点类
 */
struct StmtNode : public DummyNode {
 protected:
  StmtNode() = default;
};
/**
 * @brief if语句节点类
 */
struct IfStmtNode : public StmtNode {
 public:
  std::shared_ptr<ExprNode> cond; ///<if语句的条件表达式
  std::shared_ptr<StmtNode> then_stmt; ///<if语句的then子句
  std::shared_ptr<StmtNode> else_stmt; ///<if语句的else语句，可为空

/**
 * @brief 构造函数，将传入的参数转换为相应类型的指针并初始化成员变量
 * @param cond 赋给条件表达式节点
 * @param then_stmt 赋给if语句的then子句节点
 * @param else_stmt 赋给if语句的else子句节点
 */
  IfStmtNode(const std::shared_ptr<AbstractNode> &cond, const std::shared_ptr<AbstractNode> &then_stmt,
             const std::shared_ptr<AbstractNode> &else_stmt = nullptr)
      : cond(cast_node<ExprNode>(cond)),
        then_stmt(cast_node<StmtNode>(then_stmt)),
        else_stmt(else_stmt ? cast_node<StmtNode>(else_stmt) : nullptr) {}
/**
 * @brief 对if语句节点进行LLVM代码生成
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的LLVM值
 */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
   /**
  * @brief 判断if语句节点是否有子节点
  * @return false 没有子节点
  */
  bool should_have_children() const override { return false; }
  
  /**
   * @brief 获取if语句节点头部信息的JSON字符串
   * @return std::string 返回if语句节点头部信息的JSON字符串
   */
  std::string json_head() const override {
    return std::string{"\"type\": \"IfStmtNode\", \"cond\": "} + this->cond->to_json() +
           ", \"then_stmt\": " + this->then_stmt->to_json() +
           (else_stmt ? (", \"else_stmt\": " + this->else_stmt->to_json()) : "");
  }
};

/**
 * @brief 标识符节点类
 */
struct IdentifierNode : public LeftValueExprNode {
 public:
 /**
  * @brief 构造函数
  * @param c 标识符的名称
  */
  explicit IdentifierNode(const char *c) {
    name = std::string(c);
    std::transform(name.begin(), name.end(), name.begin(),
                   [](unsigned char c) { return std::tolower(c); });  // 为了忽略大小写，所以全部转为小写
  }

  /**
   * @brief 获取标识符的指针
   * @param context 代码生成上下文
   * @return llvm::Value* 标识符的指针
   */
  llvm::Value *get_ptr(CodegenContext &context) override;
  /**
   * @brief 获取标识符的LLVM类型
   * @param context  代码生成上下文
   * @return llvm::Type* 标识符的LLVM类型
   */
  llvm::Type *get_llvmtype(CodegenContext &context);
  
  /**
   * @brief 对标识符进行代码生成
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
  /**
  * @brief 获取标识符的JSON头
  * @return std::string 标识符的JSON头
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Identifier\", \"name\": \""} + this->name + "\"";
  }
  
  /**
   * @brief 判断是否应该有子节点
   * @return false 不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 表示数组索引访问语句的AST节点，继承自IdentifierNode
 */
struct ArrayRefNode : public IdentifierNode {
 public:
  int index;
  std::shared_ptr<ExprNode> i = nullptr;
    
  /**
   * @brief 构造函数，将传入的参数转换为相应类型的指针并初始化成员变量
   * @param c 数组名
   * @param index 数组索引
   */
  explicit ArrayRefNode(const char *c, const int index) : IdentifierNode(c), index(index) {}
  
   /**
   * @brief 构造函数，将传入的参数转换为相应类型的指针并初始化成员变量
   * @param c 数组名
   * @param i 数组索引的表达式节点
   */
  explicit ArrayRefNode(const char *c, const std::shared_ptr<AbstractNode> &i)
      : IdentifierNode(c), i(cast_node<ExprNode>(i)) {}

 /**
   * @brief 获取数组元素的指针
   * @param context 代码生成上下文
   * @return llvm::Value* 数组元素的指针
   */
  llvm::Value *get_ptr(CodegenContext &context) override;
  
  /**
   * @brief 生成代码，返回数组元素的值
   * @param context 代码生成上下文
   * @return llvm::Value* 数组元素的值
   */
  llvm::Value *codegen(CodegenContext &context) override;
  
  /**
   * @brief 获取数组索引的值
   * @param context 代码生成上下文
   * @return llvm::Value* 数组索引的值
   */
  llvm::Value *get_index(CodegenContext &context);

 protected:
  /**
   * @brief 返回JSON头部信息，包括类型和名称
   * @return std::string JSON头部信息字符串
   */
  std::string json_head() const override {
    return std::string{"\"type\": \"ArrayRefNode\", \"name\": \""} + this->name + "\"";
  }
};
/**
 * @brief 表示结构体引用节点的类
 */
struct StructRefNode : public IdentifierNode {
 public:
  std::string index;
  explicit StructRefNode(const char *c, const std::string index) : IdentifierNode(c), index(index) {}

  llvm::Value *get_ptr(CodegenContext &context) override;
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
  std::string json_head() const override {
    return std::string{"\"type\": \"ArrayRefNode\", \"name\": \""} + this->name + "\"";
  }
};
  /**
 * @brief 枚举类型，表示循环的类型
 */
enum class LoopType { REPEAT,  ///<Repeat 循环类型
                      WHILE,  ///< While 循环类型
                      FOR,  ///<For 循环类型
                      FORDOWN  ///<for_down 循环类型
                      };

/**
 * @brief 将循环类型转换为字符串
 * @param loopType 待转换的循环类型
 * @return std::string 循环类型对应的字符串
 */
enum class LoopType { REPEAT,  ///<Repeat 循环类型
                      WHILE,  ///< While 循环类型
                      FOR,  ///<For 循环类型
                      FORDOWN  ///<for_down 循环类型
                      };
/**
 * @brief 将循环类型转换为字符串
 * @param loopType 待转换的循环类型
 * @return std::string 循环类型对应的字符串
 */
inline std::string to_string(LoopType loopType) {
  std::map<LoopType, std::string> loop_to_string{{LoopType::REPEAT, "Repeat"},
                                                 {LoopType::WHILE, "While"},
                                                 {LoopType::FOR, "for"},
                                                 {LoopType::FORDOWN, "for_down"}};
  return loop_to_string[loopType];
}

  /**
 * @brief 循环语句节点
 */
struct LoopStmtNode : public StmtNode {
 public:
  LoopType type; ///<循环类型
  std::shared_ptr<IdentifierNode> i; ///<计数器标识符节点
  std::shared_ptr<ExprNode> cond; ///< 循环条件表达式节点
  std::shared_ptr<ExprNode> bound; ///< 循环边界表达式节点 
  std::shared_ptr<StmtNode> loop_stmt; ///<循环体语句节点

  /**
   * @brief 构造函数，不包含计数器
   * @param type 循环类型
   * @param cond  循环条件表达式节点
   * @param loop_stmt 循环体语句节点
   */
  LoopStmtNode(LoopType type, const std::shared_ptr<AbstractNode> &cond, const std::shared_ptr<AbstractNode> &loop_stmt)
      : type(type), cond(cast_node<ExprNode>(cond)), loop_stmt(cast_node<StmtNode>(loop_stmt)) {}
  /**
   * @brief 构造函数，包含计数器
   * @param type 循环类型
   * @param cond 循环条件表达式节点
   * @param loop_stmt 循环体语句节点
   * @param i 计数器标识符节点
   * @param bound 循环边界表达式节点
   */
  LoopStmtNode(LoopType type, const std::shared_ptr<AbstractNode> &cond, const std::shared_ptr<AbstractNode> &loop_stmt,
               std::shared_ptr<AbstractNode> &i, const std::shared_ptr<AbstractNode> &bound)
      : type(type),
        cond(cast_node<ExprNode>(cond)),
        loop_stmt(cast_node<StmtNode>(loop_stmt)),
        i(cast_node<IdentifierNode>(i)),
        bound(cast_node<ExprNode>(bound)) {}
 /**
   * @brief 生成LLVM IR代码
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
  /**
   * @brief 是否应该有子节点
   * @return false 不应该有子节点
   */
  bool should_have_children() const override { return false; }
  
  /**
   * @brief 获取JSON头部
   * @return std::string JSON头部字符串
   */
  std::string json_head() const override {
    if (type == LoopType::FOR || type == LoopType::FORDOWN)
      return std::string{"\"type\": \"" + to_string(type) + "StmtNode\", \"head\": Identifier "} + this->i->name +
             " from " + this->cond->to_json() + " to " + this->bound->to_json() +
             "\", \"stmt\": " + this->loop_stmt->to_json();
    else
      return std::string{"\"type\": \"" + to_string(type) + "StmtNode\", \"expr\": "} + this->cond->to_json() +
             ", \"stmt\": " + this->loop_stmt->to_json();
  }
};
/**
 * @brief 枚举类型，表示变量的类型
 */
enum class Type {
  /// 未定义
  UNDEFINED,
  VOID,
  /// 字符串
  STRING,
  BOOLEN,
  INTEGER,
  REAL,
  CHAR,
  ARRAY,
  STRUCT
};
/**
 * @brief 将 Type 类型转换为对应的字符串
 * @param type 要转换的 Type 类型
 * @return std::string 转换后的字符串
 */
std::string type2string(Type type);

/**
 * @brief 变量的类型节点基类
 */
struct TypeNode : public DummyNode {
  Type type = Type::UNDEFINED;
  
  /**
   * @brief 获取 LLVM 中对应的类型
   * @param context 代码生成上下文
   * @return llvm::Type* LLVM 中对应的类型
   */
  llvm::Type *get_llvm_type(CodegenContext &context) const;

  TypeNode() = default;
  
  /**
   * @brief 获取类型节点的头部 json 字符串
   * @return std::string 类型节点的头部 json 字符串
   */
  virtual std::string json_head() const = 0;

  /**
   * @brief 判断是否应该具有子节点
   * @return true 应该具有子节点
   * @return false 不应该具有子节点
   */
  virtual bool should_have_children() const = 0;
};

  /**
 * @brief 简单类型节点
 */
struct SimpleTypeNode : public TypeNode {
  using TypeNode::type;
  
  /**
   * @brief 构造函数
   * @param type 类型
   */
  SimpleTypeNode(Type type) { this->type = type; }
   /**
   * @brief 获取简单类型节点的头部 json 字符串
   * @return std::string 简单类型节点的头部 json 字符串
   */
  virtual std::string json_head() const override;
  
  /**
   * @brief 判断是否应该具有子节点
   * @return false 简单类型节点没有子节点
   */
  virtual bool should_have_children() const override { return false; }
};

  /**
 * @brief 表示字符串类型的节点
 */
struct StringTypeNode : public TypeNode {
 public:
  /**
   * @brief 构造函数，设置类型为Type::STRING
   */
  StringTypeNode() { type = Type::STRING; }
  /**
   * @brief 返回节点头部的JSON字符串
   * @return std::string 节点头部的JSON字符串
   */
  virtual std::string json_head() const override;
  /**
   * @brief 判断是否应该具有子节点
   * @return false 不应该具有子节点
   */
  virtual bool should_have_children() const override { return false; }
};
  
/**
 * @brief 表示别名类型的节点
 */
struct AliasTypeNode : public TypeNode {
 public:
  /**
   * @brief 表示子节点指针的别名
   */
  using NodePtr = std::shared_ptr<AbstractNode>;

  /**
   * @brief 标识此别名类型的标识符节点
   */
  std::shared_ptr<IdentifierNode> identifier;

  /**
   * @brief 构造函数
   * @param identifier 别名标识符节点
   */
  AliasTypeNode(const NodePtr &identifier) : identifier(cast_node<IdentifierNode>(identifier)) {}
  
  /**
   * @brief 返回节点头部的JSON字符串
   * @return std::string 节点头部的JSON字符串
   */
  virtual std::string json_head() const override;

  /**
   * @brief 判断是否应该具有子节点
   * @return false 不应该具有子节点
   */
  virtual bool should_have_children() const override { return false; }
};

  
/**
 * @brief 表示数组类型的节点
 */
struct ArrayTypeNode : public TypeNode {
 public:
  /**
  * @brief 存储元素类型的节点指针
  */
  std::shared_ptr<TypeNode> elementType;
  /**
   * @brief 存储各维度的上下界，非整数形式的上下界转换为整数存储
   */
  std::pair<int, int> bounds;
  
  /**
   * @brief 构造函数
   * @param elementType 元素类型的节点指针
   * @param bounds 存储各维度的上下界，非整数形式的上下界转换为整数存储
   */
  ArrayTypeNode(const std::shared_ptr<AbstractNode> &elementType, std::pair<int, int> bounds)
      : elementType(cast_node<TypeNode>(elementType)), bounds(bounds) {
    type = Type::ARRAY;
  }
  
  /**
   * @brief 返回节点头部的JSON字符串
   * @return std::string 节点头部的JSON字符串
   */
  virtual std::string json_head() const override;
  /**
   * @brief 判断是否应该具有子节点
   * @return false 不应该具有子节点
   */
  virtual bool should_have_children() const override { return false; }
};
/**
 * @brief 表示常量值的节点的基类
 */
struct ConstValueNode : public ExprNode {
 public:
  /**
   * @brief 获取常量值的 LLVM 类型
   * @param context 代码生成上下文
   * @return llvm::Type* 常量值的 LLVM 类型
   */
  llvm::Type *get_llvm_type(CodegenContext &context) const;

 protected:
  /**
  * @brief 构造函数
  */
  ConstValueNode() { type = nullptr; }
 /**
   * @brief 判断节点是否应该有子节点
   * @return false 不应该有子节点
   */
  bool should_have_children() const final { return false; }
};
/**
 * @brief 表示字符串常量的节点
 */
struct StringNode : public ConstValueNode {
 public:
  /**
  * @brief 字符串常量的值
  */
  std::string val;
  /**
   * @brief 构造函数
   * @param val 字符串常量的值
   */
  StringNode(const char *val) : val(val) {
    this->val.erase(this->val.begin());
    this->val.pop_back();
    type = std::make_shared<SimpleTypeNode>(Type::STRING);
  }
  /**
   * @brief 代码生成
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的 LLVM 值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:

 /**
  * @brief 获取 JSON 格式的节点头部信息
  * @return std::string JSON 格式的节点头部信息
  */
  std::string json_head() const override { return std::string{"\"type\": \"String\", \"value\": \""} + val + "\""; }
};

/**
 * @brief 布尔值常量节点类
 */
struct BoolenNode : public ConstValueNode {
 public:
 /**
  * @brief 布尔值
  */
  bool val;

  /**
   * @brief 构造函数，初始化布尔值，并设置类型为布尔类型
   * @param val 布尔值
   */
  BoolenNode(const bool val) : val(val) { type = std::make_shared<SimpleTypeNode>(Type::BOOLEN); }

  /**
   * @brief 生成代码，返回LLVM值
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:

 /**
  * @brief 返回节点的JSON头部分
  * @return std::string 节点的JSON头部分
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Boolen\", \"value\": \""} + (val == true ? "true" : "false") + "\"";
  }
};

/**
 * @brief 实数常量节点类
 */
struct RealNode : public ConstValueNode {
 public:
  double val;///<实数值*/

  /**
   * @brief 构造函数，初始化实数值，并设置类型为实数类型
   * @param val 实数值
   */
  RealNode(const double val) : val(val) { type = std::make_shared<SimpleTypeNode>(Type::REAL); }
  /**
   * @brief 构造函数，将字符串转换为实数值，并设置类型为实数类型
   * @param val 实数值字符串
   */
  RealNode(const char *val) {
    this->val = atof(val);
    type = std::make_shared<SimpleTypeNode>(Type::REAL);
  }

  /**
   * @brief 生成代码，返回LLVM值
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 返回节点的JSON头部分
  * @return std::string 节点的JSON头部分
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Real\", \"value\": \""} + std::to_string(val) + "\"";
  }
};

/**
 * @brief 整数常量节点类
 */
struct IntegerNode : public ConstValueNode {
 public:
  int val; ///<整数常量值

  /**
   * @brief 构造函数
   * @param val 整数值
   */
  IntegerNode(const int val) : val(val) { type = std::make_shared<SimpleTypeNode>(Type::INTEGER); }

  /**
   * @brief 构造函数，通过字符串初始化整数节点的值
   * @param val 字符串形式的整数节点的值
   */
  IntegerNode(const char *val) {
    this->val = atoi(val);
    type = std::make_shared<SimpleTypeNode>(Type::INTEGER);
  }

  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取整数节点的json字符串
  * @return std::string 整数节点的json字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Integer\", \"value\": \""} + std::to_string(val) + "\"";
  }
};

/**
 * @brief 字符节点
 */
struct CharNode : public ConstValueNode {
 public:
  char val;///<字符节点的值

  /**
   * @brief 构造函数，初始化字符节点的值
   * @param val 字符节点的值
   */
  CharNode(const char val) : val(val) { type = std::make_shared<SimpleTypeNode>(Type::CHAR); }
    
  /**
   * @brief 构造函数，通过字符串初始化字符节点的值
   * @param val 字符串
   */
  CharNode(const char *val) {
    this->val = *(val + 1);
    type = std::make_shared<SimpleTypeNode>(Type::CHAR);
  }
  /**
   * @brief 生成代码，返回LLVM值
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取字符节点的json字符串
  * @return std::string 字符节点的json字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"Char\", \"value\": \""} + val + "\""; }
};
/**
 * @brief Case节点列表的基类
 */
struct CaseList : public DummyNode {};
/**
 * @brief Case节点
 */
struct CaseNode : public CaseList {
 public:
  std::shared_ptr<ConstValueNode> cond; ///< Case节点的条件
  std::shared_ptr<StmtNode> stmt;///< Case节点的语句

  /**
   * @brief 构造函数，初始化Case节点的条件和语句
   * @param cond  Case节点的条件
   * @param stmt Case节点的语句
   */
  CaseNode(const std::shared_ptr<AbstractNode> &cond, const std::shared_ptr<AbstractNode> &stmt)
      : cond(cast_node<ConstValueNode>(cond)), stmt(cast_node<StmtNode>(stmt)) {}
  
  /**
   * @brief 生成该Case节点的LLVM IR代码
   * @param context 代码生成上下文
   * @return llvm::Value* 该Case节点的LLVM Value
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取该Case节点的JSON头部信息
  * @return std::string 该Case节点的JSON头部信息字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"CaseNode\", \"cond\": "} + this->cond->to_json() +
           ", \"stmt\": " + this->stmt->to_json();
  }

  /**
   * @brief 判断该Case节点是否应该有子节点
   * @return false 不应该有子节点
   */
  bool should_have_children() const override { return false; }
};
/**
 * @brief Case 语句类
 */
struct CaseStmtNode : public StmtNode {
 public:
  std::shared_ptr<ExprNode> cond; ///< 条件表达式
  std::shared_ptr<CaseList> body;  ///< 子句列表
  std::shared_ptr<StmtNode> default_stmt;  ///<默认语句

  /**
   * @brief 构造一个新的 CaseStmtNode 对象
   * @param cond 条件表达式
   * @param body Case子句列表
   * @param default_stmt 默认语句
   */
  CaseStmtNode(const std::shared_ptr<AbstractNode> &cond, const std::shared_ptr<AbstractNode> &body,
               const std::shared_ptr<AbstractNode> &default_stmt = nullptr)
      : cond(cast_node<ExprNode>(cond)),
        body(cast_node<CaseList>(body)),
        default_stmt(default_stmt ? cast_node<StmtNode>(default_stmt) : nullptr) {}
  /**
   * @brief 生成代码
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的llvm::Value指针
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 是否应该有子节点 
  * @return false 不应该有子节点
  */
  bool should_have_children() const override { return false; }
 /**
   * @brief 生成JSON头
   * @return std::string 生成的JSON头
   */
  std::string json_head() const override {
    std::string json = "\"type\": \"CaseStmtNode\", \"expr\": " + this->cond->to_json() + ", \"body\": { ";
    for (auto case_stmt : body->children()) {
      case_stmt = cast_node<CaseNode>(case_stmt);
      json += "\"case\": " + case_stmt->to_json() + ',';
    }
    if (default_stmt) json += "\"default_stmt\": " + default_stmt->to_json();
    return json;
  }
};

/**
 * @brief 二元运算符
 */
enum class BinaryOperator { GT, ///<大于 
                            GE, ///<大于等于
                            LT, ///<小于
                            LE, ///<小于等于
                            EQ, ///<等于 
                            NE, ///<不等于
                            ADD, ///<加
                            SUB, ///<减 
                            MUL, ///<乘
                            TRUEDIV, ///<真除
                            DIV, ///<整除
                            MOD, ///<取模
                            AND, ///<逻辑与
                            OR, ///<逻辑或
                            XOR ///<逻辑异或
                            };

/**
 * @brief 将二元运算符转换为字符串
 * @param binop  二元运算符
 * @return std::string 对应的字符串
 */
inline std::string to_string(BinaryOperator binop) {
  std::map<BinaryOperator, std::string> binop_to_string{
      {BinaryOperator::GT, ">"},      {BinaryOperator::GE, ">="},   {BinaryOperator::LT, "<"},
      {BinaryOperator::LE, "<="},     {BinaryOperator::EQ, "="},    {BinaryOperator::NE, "<>"},
      {BinaryOperator::ADD, "+"},     {BinaryOperator::SUB, "-"},   {BinaryOperator::MUL, "*"},
      {BinaryOperator::TRUEDIV, "/"}, {BinaryOperator::DIV, "div"}, {BinaryOperator::MOD, "mod"},
      {BinaryOperator::AND, "and"},   {BinaryOperator::OR, "or"},   {BinaryOperator::XOR, "xor"}};
  // TODO: bound checking
  return binop_to_string[binop];
}
/**
 * @brief  二元表达式节点，继承自表达式节点 ExprNode
 */
struct BinopExprNode : public ExprNode {
 public:
  BinaryOperator op; ///< 二元运算符
  std::shared_ptr<ExprNode> lhs; ///< 左操作数
  std::shared_ptr<ExprNode> rhs; ///< 右操作数

  /**
   * @brief 构造一个二元表达式节点
   * @param op 二元运算符
   * @param lhs 左操作数
   * @param rhs 右操作数
   */
  BinopExprNode(BinaryOperator op, const std::shared_ptr<AbstractNode> &lhs, const std::shared_ptr<AbstractNode> &rhs)
      : op(op), lhs(cast_node<ExprNode>(lhs)), rhs(cast_node<ExprNode>(rhs)) {}

  /**
   * @brief 代码生成函数，生成该节点的 LLVM IR 代码
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的 LLVM IR 值
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 该节点是否应该包含子节点
  * @return false 该节点不应该包含子节点
  */
  bool should_have_children() const override { return false; }

  /**
   * @brief 返回该节点的 JSON 格式的头部信息
   * @return std::string JSON 格式的头部信息
   */
  std::string json_head() const override {
    return std::string{"\"type\": \"BinopExpr\", \"op\": \""} + to_string(this->op) +
           "\", \"lhs\": " + this->lhs->to_json() + ", \"rhs\": " + this->rhs->to_json();
  }
};

/**
 * @brief 函数表达式节点
 */
struct FuncExprNode : public ExprNode {
 public:
  std::shared_ptr<AbstractNode> func_call;///<指向AST节点的智能指针
  /**
   * @brief 构造函数
   * @param func_call 函数调用节点的智能指针，必须是 RoutineCallNode 或 SysCallNode 的子类
   */
  FuncExprNode(const std::shared_ptr<AbstractNode> &func_call) : func_call(func_call) {
    assert(is_a_ptr_of<RoutineCallNode>(func_call) || is_a_ptr_of<SysCallNode>(func_call));
  }

  /**
   * @brief 生成该AST节点对应的LLVM IR代码
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM IR值的指针
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
  /**
   * @brief 获取JSON字符串的头部信息
   * @return std::string JSON字符串的头部信息
   */
  std::string json_head() const override {
    return std::string{"\"type\": \"FuncExpr\", \"call\": "} + this->func_call->to_json();
  }

  /**
   * @brief 该节点是否应该有子节点
   * @return false 该节点不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 枚举类型，包含系统例程的名称
 */
enum class SysRoutine {
  WRITELN, ///<输出并回车
  WRITE, ///<输出
  READ, ///<读取
  READLN, ///<读取并回车
  SQRT, ///<开方
  ABS, ///<绝对值
  ORD, ///<字符转 ASCII 码
  PRED, ///<前驱
  SUCC, ///<后继
  CHR ///<ASCII 码转字符
};

/**
 * @brief 将 SysRoutine 类型转换为对应的字符串
 * @param routine 系统例程SysRoutine类型
 * @return std::string 系统例程名称对应的字符串表示
 */
inline std::string to_string(SysRoutine routine) {
  std::map<SysRoutine, std::string> routine_to_string{{SysRoutine::WRITELN, "writeln"}, {SysRoutine::WRITE, "write"},
                                                      {SysRoutine::READ, "read"},       {SysRoutine::READLN, "readln"},
                                                      {SysRoutine::SQRT, "sqrt"},       {SysRoutine::ABS, "abs"},
                                                      {SysRoutine::ORD, "ord"},         {SysRoutine::PRED, "pred"},
                                                      {SysRoutine::SUCC, "succ"},       {SysRoutine::CHR, "chr"}};
  // TODO: bound checking
  return routine_to_string[routine];
}
/**
 * @brief 系统例程类型节点
 */
struct SysRoutineNode : public DummyNode {
 public:
  SysRoutine routine;

  /**
   * @brief 构造函数
   * @param routine 系统例程类型
   */
  explicit SysRoutineNode(SysRoutine routine) : routine(routine) {}

 protected:
 /**
   * @brief 该节点是否应该有子节点
   * @return false 该节点不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 系统例程调用节点
 */
struct SysCallNode : public DummyNode {
 public:
  std::shared_ptr<SysRoutineNode> routine; ///<系统例程节点
  std::shared_ptr<ArgListNode> args; ///<系统例程参数节点

  /**
   * @brief 系统例程调用节点构造函数
   * @param routine 系统例程节点
   * @param args 系统例程参数节点
   */
  SysCallNode(const std::shared_ptr<AbstractNode> &routine, const std::shared_ptr<AbstractNode> &args);
  /**
   * @brief 系统例程调用节点构造函数
   * @param routine 系统例程节点
   */
  explicit SysCallNode(const std::shared_ptr<AbstractNode> &routine) : SysCallNode(routine, make_node<ArgListNode>()) {}
  //explicit关键字声明只有一个函数的构造函数，不能用于隐式类型转换，可以防止隐式类型转换发生

  /**
   * @brief 生成该AST节点对应的LLVM IR代码
   * @param context 代码生成上下文
   * @return llvm::Value* 生成的LLVM IR值的指针
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
   * @brief 获取JSON字符串的头部信息
   * @return std::string JSON字符串的头部信息
   */
  std::string json_head() const override;

  /**
   * @brief 该节点是否应该有子节点
   * @return false 该节点不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 参数列表节点
 */
struct ArgListNode : public DummyNode {
 protected:
 /**
  * @brief 返回JSON字符串的头部，表示该节点的类型信息
  * @return std::string JSON字符串的头部
  */
  std::string json_head() const override { return std::string{"\"type\": \"ArgList\""}; }
  /**
   * @brief 检查该节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const override { return true; }
};

/**
 * @brief 参数声明节点类，用于表示函数或过程的形参声明。
 */
struct ParamDeclNode : public DummyNode {
 public:
  using NodePtr = std::shared_ptr<AbstractNode>;
  /// 程序名
  std::shared_ptr<IdentifierNode> name;
  /// 返回类型
  std::shared_ptr<TypeNode> type;
  /**
   * @brief 构造函数
   * @param name 参数名节点
   * @param type 参数类型节点
   */
  ParamDeclNode(const NodePtr &name, const NodePtr &type)
      : name(cast_node<IdentifierNode>(name)), type(cast_node<TypeNode>(type)) {
    assert(is_a_ptr_of<SimpleTypeNode>(type) || is_a_ptr_of<AliasTypeNode>(type));
  }

 protected:
 /**
  * @brief 返回JSON字符串的头部，表示该节点的类型信息及其子节点信息
  * @return std::string JSON字符串的头部
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"ParamDecl\", \"name\": "} + this->name->to_json() +
           ", \"decl\": " + this->type->to_json();
  }

  /**
   * @brief 检查该节点是否应该有子节点
   * @return false 该节点不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 程序列表语义节点
 */
struct ParamListNode : public DummyNode {
 protected:
 /**
  * @brief 返回节点的 JSON 格式头部
  * @return std::string 节点的 JSON 格式头部
  */
  std::string json_head() const override { return std::string{"\"type\": \"ParamList\""}; }

  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const override { return true; }
};
/**
 * @brief 变量声明节点
 */
struct VarDeclNode : public DummyNode {
 public:
  /// 变量名
  std::shared_ptr<IdentifierNode> name;
  /// 变量类型
  std::shared_ptr<TypeNode> type;

  /**
   * @brief 构造一个新的 VarDeclNode对象
   * @param name 变量名
   * @param type 变量类型
   */
  VarDeclNode(const std::shared_ptr<AbstractNode> &name, const std::shared_ptr<AbstractNode> &type)
      : name(cast_node<IdentifierNode>(name)), type(cast_node<TypeNode>(type)) {}
  /**
   * @brief 生成 LLVM IR 代码
   * @param context 代码生成上下文
   * @return llvm::Value* LLVM IR Value 对象
   */
  llvm::Value *codegen(CodegenContext &context) override;
  /**
   * @brief 返回节点的 JSON 格式头部
   * @return std::string 节点的 JSON 格式头部
   */
 protected:
  std::string json_head() const override {
    return std::string{"\"type\": \"VarDecl\", \"name\": "} + this->name->to_json() +
           ", \"decl\": " + this->type->to_json();
  }
  /**
   * @brief 判断节点是否应该有子节点
   * @return false 节点不应该有子节点
   */
  bool should_have_children() const override { return false; }
};

/**
 * @brief 变量列表节点
 */
struct VarListNode : public DummyNode {
 public:
 /**
  * @brief 对变量列表进行LLVM IR代码生成
  * @param context 代码生成上下文
  * @return llvm::Value* 代码生成结果
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取JSON字符串的头部
  * @return std::string JSON头部字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"VarList\""}; }

  /**
   * @brief 判断该节点是否应具有子节点
   * @return true 该节点始终具有子节点
   */
  bool should_have_children() const override { return true; }
};

/**
 * @brief 常量声明节点
 */
struct ConstDeclNode : public DummyNode {
 public:
  /// 常量名
  std::shared_ptr<IdentifierNode> name;
  /// 常量值
  std::shared_ptr<ConstValueNode> value;

  /**
   * @brief 构造一个新的常量声明节点
   * @param name 常量名
   * @param value 常量值
   */
  ConstDeclNode(const std::shared_ptr<AbstractNode> &name, const std::shared_ptr<AbstractNode> &value)
      : name(cast_node<IdentifierNode>(name)), value(cast_node<ConstValueNode>(value)) {}

  /**
   * @brief 对常量声明进行LLVM IR代码生成
   * @param context 代码生成上下文
   * @return llvm::Value* 代码生成结果
   */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取JSON字符串的头部
  * @return std::string JSON头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"ConstDecl\", \"name\": "} + this->name->to_json() +
           ", \"value\": " + this->value->to_json();
  }

  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const override { return false; }
};
/**
 * @brief 常量列表节点
 */
struct ConstListNode : public DummyNode {
 public:
 /**
  * @brief  节点的LLVM代码生成方法
  * @param context 代码生成上下文
  * @return llvm::Value* 代码生成结果
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取JSON字符串的头部
  * @return std::string JSON头部字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"ConstList\""}; }

  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const override { return true; }
};
/**
 * @brief 类型定义节点
 */
struct TypeDefNode : public DummyNode {
 public:
  std::shared_ptr<IdentifierNode> name;///<类型别名标识符节点
  std::shared_ptr<TypeNode> type; ///<类型别名节点
  /**
   * @brief 构造函数
   * @param name 类型别名标识符节点
   * @param type 类型别名节点
   */
  TypeDefNode(const std::shared_ptr<AbstractNode> &name, const std::shared_ptr<AbstractNode> &type)
      : name(cast_node<IdentifierNode>(name)), type(cast_node<TypeNode>(type)) {}
    
  /**
  * @brief  节点的LLVM代码生成方法
  * @param context 代码生成上下文
  * @return llvm::Value* 代码生成结果
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取JSON字符串的头部
  * @return std::string JSON头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"TypeDef\", \"name\": "} + this->name->to_json() +
           ", \"alias\": " + this->type->to_json();
  }

  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const override { return false; }
};
/**
 * @brief 类型列表语义节点
 */
struct TypeListNode : public DummyNode {
 public:
 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"TypeList\""}; }

  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const override { return true; }
};

struct NameListNode : public DummyNode {};
/**
 * @brief 过程调用语义节点
 */
struct RoutineCallNode : public DummyNode {
 public:
  /// 函数名
  std::shared_ptr<IdentifierNode> identifier;
  /// 实参
  std::shared_ptr<ArgListNode> args;

  /**
   * @brief 构造函数
   * @param 函数名
   * @param 实参 
   */
  RoutineCallNode(const std::shared_ptr<AbstractNode> &identifier, const std::shared_ptr<AbstractNode> &args)
      : identifier(cast_node<IdentifierNode>(identifier)), args(cast_node<ArgListNode>(args)) {}

  /**
   * @brief 构造函数
   * @param identifier 函数名
   */
  explicit RoutineCallNode(const std::shared_ptr<AbstractNode> &identifier)
      : RoutineCallNode(identifier, make_node<ArgListNode>()) {}
 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"RoutineCall\", \"identifier\": "} + this->identifier->to_json() +
           ", \"args\": " + this->args->to_json();
  }

  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const final { return false; }
};


/**
 * @brief 子过程列表节点
 */
struct SubroutineListNode : public DummyNode {
 public:
 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"SubroutineList\""}; }

  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const final { return true; }
};
/**
 * @brief 头部列表语义节点
 */
struct HeadListNode : public DummyNode {
 public:
  using NodePtr = std::shared_ptr<AbstractNode>;
  std::shared_ptr<ConstListNode> const_list; ///<常量列表
  std::shared_ptr<TypeListNode> type_list; ///<类型列表
  std::shared_ptr<VarListNode> var_list; ///<变量列表
  std::shared_ptr<SubroutineListNode> subroutine_list; ///<子程序列表
  /**
   * @brief 构造函数
   * @param consts 常量列表节点
   * @param types 类型列表节点
   * @param vars 变量列表节点
   * @param subroutines 子程序列表节点
   */
  HeadListNode(const NodePtr &consts, const NodePtr &types, const NodePtr &vars, const NodePtr &subroutines)
      : const_list(cast_node<ConstListNode>(consts)),
        type_list(cast_node<TypeListNode>(types)),
        var_list(cast_node<VarListNode>(vars)),
        subroutine_list(cast_node<SubroutineListNode>(subroutines)) {}
  /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"HeadList\", \"consts\": "} + this->const_list->to_json() +
           ", \"types\": " + this->type_list->to_json() + ", \"vars\": " + this->var_list->to_json() +
           ", \"subroutines\": " + this->subroutine_list->to_json();
  }
  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const final { return false; }
};

/**
 * @brief 过程语义节点
 */
struct RoutineNode : public DummyNode {
 public:
  using NodePtr = std::shared_ptr<AbstractNode>;
  std::shared_ptr<IdentifierNode> name; ///<过程名称
  std::shared_ptr<HeadListNode> head_list; ///<过程头部节点

/**
 * @brief 构造函数
 * @param name 过程名
 * @param head_list 过程头部节点
 */
  RoutineNode(const NodePtr &name, const NodePtr &head_list)
      : name(cast_node<IdentifierNode>(name)), head_list(cast_node<HeadListNode>(head_list)) {}

 protected:
  RoutineNode() = default;
  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const final { return true; }
};
/**
 * @brief 函数语义节点
 */
struct ProgramNode : public RoutineNode {
 public:
  using RoutineNode::RoutineNode;

 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Program\", \"name\": "} + this->name->to_json() +
           ", \"head\": " + this->head_list->to_json();
  }
};

/**
 * @brief 子程序语义节点，包括函数和过程
 */
struct SubroutineNode : public RoutineNode {
 public:
  std::shared_ptr<ParamListNode> params; ///<参数列表
  std::shared_ptr<TypeNode> return_type; ///<返回类型

  /**
   * @brief 构造函数
   * @param name 名字
   * @param params 参数列表
   * @param type 返回值类型
   * @param head_list 头部信息列表
   */
  SubroutineNode(const NodePtr &name, const NodePtr &params, const NodePtr &type, const NodePtr &head_list)
      : RoutineNode(name, head_list), params(cast_node<ParamListNode>(params)), return_type(cast_node<TypeNode>(type)) {
    assert(is_a_ptr_of<SimpleTypeNode>(type) || is_a_ptr_of<AliasTypeNode>(type));
  }
 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief 获取节点的 JSON 头部
  * @return std::string 节点的 JSON 头部字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"Subroutine\", \"name\": "} + this->name->to_json() +
           ", \"params\": " + this->params->to_json() + ", \"return\": " + this->return_type->to_json() +
           ", \"head\": " + this->head_list->to_json();
  }
};
/**
 * @brief 复合语句节点
 */
struct CompoundStmtNode : public StmtNode {
 protected:
 /**
  * @brief  获取JSON头部信息
  * @return std::string JSON头部信息字符串
  */
  std::string json_head() const override { return std::string{"\"type\": \"CompoundStmt\""}; }

  /**
   * @brief 判断节点是否应该有子节点
   * @return true 该节点始终有子节点
   */
  bool should_have_children() const override { return true; }
   
 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;
};
/**
 * @brief 表示赋值语句的节点
 */
struct AssignStmtNode : public StmtNode {
  using NodePtr = std::shared_ptr<AbstractNode>;

 public:
  /// 被赋值量
  std::shared_ptr<ExprNode> lhs;
  /// 赋值量
  std::shared_ptr<ExprNode> rhs;

  /**
   * @brief 构造函数
   * @param lhs 被赋值量
   * @param rhs 赋值量
   */
  AssignStmtNode(const NodePtr &lhs, const NodePtr &rhs)
      : lhs(cast_node<ExprNode>(lhs)), rhs(cast_node<ExprNode>(rhs)) {
    assert(is_a_ptr_of<IdentifierNode>(lhs) || is_a_ptr_of<ArrayRefNode>(lhs) /* || is_a_ptr_of<RecordRefNode>(lhs) */);
  }
  /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief  获取JSON头部信息
  * @return std::string JSON头部信息字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"AssignStmt\", \"lhs\": "} + this->lhs->to_json() +
           ", \"rhs\": " + this->rhs->to_json();
  }

  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const override { return false; }
};
/**
 * @brief 过程调用语句节点
 */
struct ProcStmtNode : public StmtNode {
 public:
  std::shared_ptr<AbstractNode> proc_call;///< 过程调用节点
/**
 * @brief 构造函数
 * @param proc_call 过程调用节点
 */
  ProcStmtNode(const std::shared_ptr<AbstractNode> &proc_call) : proc_call(proc_call) {
    assert(is_a_ptr_of<RoutineCallNode>(proc_call) || is_a_ptr_of<SysCallNode>(proc_call));
  }

 /**
  * @brief  生成 LLVM IR 代码
  * @param context 代码生成上下文
  * @return llvm::Value*  生成的 LLVM 值
  */
  llvm::Value *codegen(CodegenContext &context) override;

 protected:
 /**
  * @brief  获取JSON头部信息
  * @return std::string JSON头部信息字符串
  */
  std::string json_head() const override {
    return std::string{"\"type\": \"ProcStmt\", \"call\": "} + this->proc_call->to_json();
  }
  /**
   * @brief 判断该节点是否应具有子节点
   * @return false 该节点没有子节点
   */
  bool should_have_children() const override { return false; }
};
/**
 * @brief 表示语句列表的节点
 */
struct StmtList : public StmtNode {};
  
/**
 * @brief 表示记录类型的节点
 */
struct RecordTypeNode : public TypeNode {
 public:
  using NodePtr = std::shared_ptr<AbstractNode>;
  /**
   * @brief 记录类型中字段名称和下标的映射关系
   */
  std::map<std::string, int> index;
  /**
   * @brief 记录类型中各字段类型的vector
   */
  std::vector<std::shared_ptr<TypeNode>> types;

  /**
   * @brief 构造函数，用于从类型定义节点构造记录类型节点
   * @param types 类型定义节点
   */
  RecordTypeNode(const NodePtr &types) {
    int i = 0;
    for (auto &child : types->children()) {
      index[cast_node<TypeDefNode>(child)->name->name] = i++;
      this->types.push_back(cast_node<TypeDefNode>(child)->type);
    }
    this->type = Type::STRUCT;
  }

  /**
   * @brief 生成记录类型节点的JSON头部
   * @return std::string 记录类型节点的JSON头部字符串
   */
  virtual std::string json_head() const override;
  /**
   * @brief 判断记录类型节点是否应该包含子节点
   * @return false 不应该包含子节点
   */
  virtual bool should_have_children() const override { return false; }
};

}  // namespace spc

#endif
