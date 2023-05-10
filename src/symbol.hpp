#ifndef SYMBOL_H
#define SYMBOL_H
#include <map>
#include "ast.hpp"

namespace spc {
struct SymbolTable;
struct CodegenContext;
/// 符号实体 用来存放变量与其对应的指针
struct Symbol {
  std::string name;   ///< 符号名称
  bool isConst;///< 符号是否为常量 
  llvm::Value *ptr;///<符号对应的指针
  std::shared_ptr<TypeNode> typeNode;///<符号对应的类型
  
  /**
   * @brief 获取符号对应的指针
   * @return llvm::Value* 符号对应的指针
   */
  llvm::Value *get_llvmptr() { return ptr; }
  
  /**
   * @brief 获取符号对应的 LLVM 类型
   * @param context 代码生成上下文
   * @return llvm::Type*  符号对应的 LLVM 类型
   */
  llvm::Type *get_llvmtype(CodegenContext &context) { return typeNode->get_llvm_type(context); }
  /**
   * @brief 构造函数
   * @param name 符号名称
   * @param type 符号对应的类型
   * @param ptr 符号对应的指针
   * @param isConst 符号是否为常量
   */
  Symbol(std::string name, std::shared_ptr<TypeNode> type, llvm::Value *ptr, bool isConst = false)
      : name(name), typeNode(type), ptr(ptr), isConst(isConst) {}
  friend struct SymbolTable;
};
/**
 * @brief 符号表
 */
struct SymbolTable {
  /**
   * @brief 构造函数
   * @param context 代码生成上下文
   */
  SymbolTable(CodegenContext *context) : context(*context) {}
    
  /**
   * @brief  向局部符号表中添加符号
   * @param name 符号名称
   * @param isConst 符号是否为常量
   * @return true 添加成功
   * @return false 添加失败
   */
  bool addLocalSymbol(std::string name, std::shared_ptr<TypeNode>, bool isConst = false);
  /**
   * @brief 获取局部符号表中的符号
   * @param name 符号名称
   * @return std::shared_ptr<Symbol> 符号对应的指针
   */
  std::shared_ptr<Symbol> getLocalSymbol(std::string name);
 
  /**
   * @brief 向全局符号表中添加符号
   * @param name 符号名称
   * @param initializer 符号对应的类型
   * @param isConst 符号是否为常量
   * @return true 添加成功
   * @return false 添加失败
   */
  bool addGlobalSymbol(std::string name, std::shared_ptr<TypeNode>, llvm::Constant *initializer, bool isConst = false);
  /**
   * @brief 获取全局符号表中的符号
   * @param name 符号名称
   * @return std::shared_ptr<Symbol> 符号对应的指针
   */
  std::shared_ptr<Symbol> getGlobalSymbol(std::string name);
  
  /**
   * @brief 添加局部别名
   * @param alias 别名名称
   * @param type 别名对应的类型
   * @return true 添加成功
   * @return false 添加失败
   */
  bool addLocalAlias(std::string alias, std::shared_ptr<TypeNode> type);
    
  /**
   * @brief 获取局部别名
   * @param name 别名名称
   * @return std::shared_ptr<TypeNode> 返回对应的别名指针，若不存在返回nullptr
   */
  std::shared_ptr<TypeNode> getLocalAlias(std::string name);
  
  /**
   * @brief 添加全局别名符号
   * @param alias 别名名称
   * @param type 别名类型
   * @return true 添加成功
   * @return false 添加失败
   */
  bool addGlobalAlias(std::string alias, std::shared_ptr<TypeNode> type);
  
    
  /**
   * @brief 获取全局别名符号
   * @param name 别名名称
   * @return std::shared_ptr<TypeNode> 返回别名类型指针
   */
  std::shared_ptr<TypeNode> getGlobalAlias(std::string name);
  /**
   * @brief 重置局部变量表
   */
  void resetLocals();

 protected:
  /// 局部变量表
  std::map<std::string, std::shared_ptr<Symbol>> localSymbols;
  /// 全局变量表
  std::map<std::string, std::shared_ptr<Symbol>> globalSymbols;
  /// 局部别名表
  std::map<std::string, std::shared_ptr<TypeNode>> localAliases;
  /// 全局别名表
  std::map<std::string, std::shared_ptr<TypeNode>> globalAliases;
  CodegenContext &context;
};
}  // namespace spc

#endif
