#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
// used by routine nodes
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Support/Casting.h>
// used by decl nodes
#include <fmt/core.h>

#include "ast.hpp"
#include "context.hpp"
#include "symbol.hpp"

namespace spc {
/* -------- syscall nodes -------- */
/**
 * @brief 生成代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的值
 */
llvm::Value *SysCallNode::codegen(CodegenContext &context) {
  switch (routine->routine) {
    case SysRoutine::WRITELN:
    case SysRoutine::WRITE: {
      auto char_ptr = context.builder.getInt8Ty()->getPointerTo();
      auto printf_type = llvm::FunctionType::get(context.builder.getInt32Ty(), char_ptr, true);
      auto printf_func = context.module->getOrInsertFunction("printf", printf_type);
      for (auto &arg : args->children()) {
        auto value = arg->codegen(context);
        std::vector<llvm::Value *> args;
        auto type = value->getType();
        if (type->isArrayTy()) type = type->getArrayElementType();
        if (type->isIntegerTy(8)) {
          args.push_back(context.builder.CreateGlobalStringPtr("%c"));
          args.push_back(value);
        } else if (type->isIntegerTy()) {
          args.push_back(context.builder.CreateGlobalStringPtr("%d"));
          args.push_back(value);
        } else if (type->isDoubleTy()) {
          args.push_back(context.builder.CreateGlobalStringPtr("%lf"));
          args.push_back(value);
        }
        // Pascal pointers are not supported, so this is an LLVM global string pointer.
        else if (type->isPointerTy()) {
          args.push_back(value);
        } else {
          if (routine->routine == SysRoutine::WRITE)
            throw CodegenException("incompatible type in write(): expected char, integer, real");
          else
            throw CodegenException("incompatible type in writeln(): expected char, integer, real");
        }
        context.builder.CreateCall(printf_func, args);
      }
      if (routine->routine == SysRoutine::WRITELN) {
        context.builder.CreateCall(printf_func, context.builder.CreateGlobalStringPtr("\n"));
      }
      return nullptr;
    }

    case SysRoutine::READ:
    case SysRoutine::READLN: {
      auto char_ptr = context.builder.getInt8Ty()->getPointerTo();
      auto scanf_type = llvm::FunctionType::get(context.builder.getInt32Ty(), char_ptr, true);
      auto scanf_func = context.module->getOrInsertFunction("scanf", scanf_type);
      for (auto &arg : args->children()) {
        std::vector<llvm::Value *> args;
        /*添加空指针判断*/
        auto ptr = cast_node<IdentifierNode>(arg)->get_ptr(context);
        if (ptr == nullptr) throw CodegenException("identifier not found: " + cast_node<IdentifierNode>(arg)->name);
        auto type = cast_node<IdentifierNode>(arg)->get_llvmtype(context);
        if (type->isArrayTy()) {
          ptr = cast_node<ArrayRefNode>(arg)->get_ptr(context);
          type = type->getArrayElementType();
        }
        if (type->isIntegerTy(8)) {
          args.push_back(context.builder.CreateGlobalStringPtr("%c"));
          args.push_back(ptr);
        } else if (type->isIntegerTy()) {
          args.push_back(context.builder.CreateGlobalStringPtr("%d"));
          args.push_back(ptr);
        } else if (type->isDoubleTy()) {
          args.push_back(context.builder.CreateGlobalStringPtr("%lf"));
          args.push_back(ptr);
        } else {
          if (routine->routine == SysRoutine::READ)
            throw CodegenException("incompatible type in read(): expected char, integer, real");
          else
            throw CodegenException("incompatible type in readln(): expected char, integer, real");
        }
        context.builder.CreateCall(scanf_func, args);
      }
      if (routine->routine == SysRoutine::READLN) {
        context.builder.CreateCall(scanf_func, context.builder.CreateGlobalStringPtr("%*[^\n]"));
        auto getchar_type = llvm::FunctionType::get(context.builder.getInt32Ty(), false);
        auto getchar_func = context.module->getOrInsertFunction("getchar", getchar_type);
        context.builder.CreateCall(getchar_func);
      }
      return nullptr;
    }

    case SysRoutine::SQRT: {
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'sqrt'");
      auto sqrt_type = llvm::FunctionType::get(context.builder.getDoubleTy(), context.builder.getDoubleTy(), false);
      auto sqrt_func = context.module->getOrInsertFunction("sqrt", sqrt_type);
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!(value->getType()->isIntegerTy(32) || value->getType()->isDoubleTy())) {
        throw CodegenException("incompatible type in sqrt(): expected integer, real");
      }
      std::vector<llvm::Value *> args;
      if (value->getType()->isIntegerTy(32)) value = context.builder.CreateUIToFP(value, context.builder.getDoubleTy());
      args.push_back(value);
      return context.builder.CreateCall(sqrt_func, args);
    }
    case SysRoutine::ABS: {
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'abs'");
      auto fabs_type = llvm::FunctionType::get(context.builder.getDoubleTy(), context.builder.getDoubleTy(), false);
      auto fabs_func = context.module->getOrInsertFunction("fabs", fabs_type);
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!(value->getType()->isIntegerTy(32) || value->getType()->isDoubleTy()))
        throw CodegenException("incompatible type in abs(): expected integer, real");
      std::vector<llvm::Value *> args;
      if (value->getType()->isIntegerTy(32)) value = context.builder.CreateUIToFP(value, context.builder.getDoubleTy());
      args.push_back(value);
      return context.builder.CreateCall(fabs_func, args);
    }
    case SysRoutine::ORD: {
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'ord'");
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!value->getType()->isIntegerTy(8)) throw CodegenException("incompatible type in ord(): expected char");
      return context.builder.CreateZExt(value, context.builder.getInt32Ty());
    }
    case SysRoutine::PRED: {
      //只支持字符和整数
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'pred'");
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!(value->getType()->isIntegerTy(8) || value->getType()->isIntegerTy(32)))
        throw CodegenException("incompatible type in pred(): expected char, int");
      if (value->getType()->isIntegerTy(32))
        return context.builder.CreateSub(value, context.builder.getInt32(1));
      else
        return context.builder.CreateSub(value, context.builder.getInt8(1));
    }
    case SysRoutine::SUCC: {
      //只支持字符和整数
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'succ'");
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!(value->getType()->isIntegerTy(8) || value->getType()->isIntegerTy(32)))
        throw CodegenException("incompatible type in succ(): expected char, int");
      if (value->getType()->isIntegerTy(32))
        return context.builder.CreateAdd(value, context.builder.getInt32(1));
      else
        return context.builder.CreateAdd(value, context.builder.getInt8(1));
    }
    case SysRoutine::CHR: {
      if (args->children().size() != 1) throw CodegenException("no matching function for call to 'chr'");
      auto arg = *args->children().begin();
      auto value = arg->codegen(context);
      if (!value->getType()->isIntegerTy(32)) throw CodegenException("incompatible type in chr(): expected int");
      return context.builder.CreateTrunc(value, context.builder.getInt8Ty());
    }
    default:
      throw CodegenException("unsupported built-in routine: " + to_string(routine->routine));
  }
  return nullptr;
}

/* -------- type nodes -------- */
/**
 * @brief 将AST类型转换为LLVM类型
 * @param type  AST中的类型
 * @param context 代码生成上下文
 * @return llvm::Type* 返回LLVM中对应的类型
 * @retval context.builder.getInt1Ty() 布尔类型
 * @retval context.builder.getInt32Ty() 整型
 * @retval context.builder.getDoubleTy() 双精度浮点型
 * @retval context.builder.getInt8Ty() 字符类型
 * @retval context.builder.getVoidTy() 空类型
 * @retval nullptr 不支持的类型
 */
static llvm::Type *llvm_type(Type type, CodegenContext &context) {
  switch (type) {
    case Type::BOOLEN:
      return context.builder.getInt1Ty();
    case Type::INTEGER:
      return context.builder.getInt32Ty();
    case Type::REAL:
      return context.builder.getDoubleTy();
    case Type::CHAR:
      return context.builder.getInt8Ty();
    case Type::VOID:
      return context.builder.getVoidTy();
    default:
      return nullptr;
  }
}
/**
 * @brief 将自定义类型转换为 LLVM IR 类型
 * @param type 自定义类型
 * @param length 数组类型的长度
 * @param context 代码生成上下文
 * @return llvm::Type* 对应的 LLVM IR 类型
 */
static llvm::Type *llvm_type(Type type, int length, CodegenContext &context) {
  return llvm::ArrayType::get(llvm_type(type, context), length);
}
static llvm::Type *llvm_type(std::vector<std::shared_ptr<TypeNode>> types, CodegenContext &context) {
  llvm::StructType *structType = llvm::StructType::get(context.module->getContext());
  std::vector<llvm::Type *> elements;
  for (auto &type : types) elements.push_back(type->get_llvm_type(context));
  structType->setBody(elements);
  return structType;
}

/**
 * @brief 获取常量节点的 LLVM IR 类型
 * @param context 代码生成上下文
 * @return llvm::Type* 常量节点的 LLVM IR 类型
 */
llvm::Type *ConstValueNode::get_llvm_type(CodegenContext &context) const { return this->type->get_llvm_type(context); }

/**
 * @brief 获取类型节点的 LLVM IR 类型
 * 如果是简单类型节点，则直接转换为 LLVM IR 类型。
 * 如果是数组类型节点，则转换为 LLVM 数组类型。
 * 如果是类型别名节点，则获取其全局别名节点，并返回其 LLVM IR 类型。
 * 如果不是上述任何一种类型，则抛出 CodegenException 异常。
 * @param context 代码生成上下文
 * @return llvm::Type* 类型节点的 LLVM IR 类型
 * @throw CodegenException 如果类型不支持，则抛出异常
 */
llvm::Type *TypeNode::get_llvm_type(CodegenContext &context) const {
  if (auto *simple_type = dynamic_cast<const SimpleTypeNode *>(this)) {
    return llvm_type(simple_type->type, context);
  } else if (auto *array_type = dynamic_cast<const ArrayTypeNode *>(this)) {
    auto length = array_type->bounds.second - array_type->bounds.first + 1;
    return llvm_type(array_type->elementType->type, length, context);
  } else if (auto *alias = dynamic_cast<const AliasTypeNode *>(this)) {
    return context.symbolTable.getGlobalAlias(alias->identifier->name)->get_llvm_type(context);
  } else if (auto *struct_type = dynamic_cast<const RecordTypeNode *>(this)) {
    return llvm_type(struct_type->types, context);
  }

  throw CodegenException("unsupported type: " + type2string(type));
  return nullptr;  // 这里永远不会运行
}
/* -------- routine call nodes -------- */
/**
 * @brief 生成过程调用语义节点对应LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回调用结果的 LLVM 值
 * @remarks 该函数通过使用上下文模块中的函数名称查找函数并获取其参数数量，然后检查其与当前函数调用中传递的参数数量是否匹配。
 * 如果不匹配，则会抛出异常。
 * 接下来，它会通过循环对每个参数调用codegen方法并将其添加到值列表中。最后，使用上下文生成器对象调用函数，并将结果返回。
 */
llvm::Value *RoutineCallNode::codegen(CodegenContext &context) {
  auto *func = context.module->getFunction(identifier->name);
  if (func->arg_size() != args->children().size()) {
    throw CodegenException("wrong number of arguments: " + identifier->name + "()");
  }
  std::vector<llvm::Value *> values;
  // TODO: check type compatibility between arguments and parameters
  for (auto &arg : args->children()) values.push_back(arg->codegen(context));
  return context.builder.CreateCall(func, values);
}
/* -------- routine nodes -------- */

/**
 * @brief 生成函数语义节点对应的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的LLVM IR值
 * @remarks 该函数用于生成程序代码的LLVM IR。函数通过遍历代码树中的子节点生成常量、类型、变量和子程序，
 * 之后创建一个名为"main"的函数作为程序的入口，并将代码树中的所有语句转换为该函数中的基本块，
 * 最后返回生成的LLVM IR值。
 */
llvm::Value *ProgramNode::codegen(CodegenContext &context) {
  context.is_subroutine = false;
  head_list->const_list->codegen(context);
  head_list->type_list->codegen(context);
  head_list->var_list->codegen(context);

  context.is_subroutine = true;
  head_list->subroutine_list->codegen(context);
  context.is_subroutine = false;

  auto *func_type = llvm::FunctionType::get(context.builder.getInt32Ty(), false);
  auto *main_func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "main", context.module.get());
  auto *block = llvm::BasicBlock::Create(context.module->getContext(), "entry", main_func);
  context.builder.SetInsertPoint(block);
  for (auto &stmt : children()) stmt->codegen(context);
  context.builder.CreateRet(context.builder.getInt32(0));

  llvm::verifyFunction(*main_func);
  if (context.fpm) {
    context.fpm->run(*main_func);
  }
  if (context.mpm) {
    context.mpm->run(*context.module);
  }
  return nullptr;
}

/**
 * @brief 生成子程序语义节点对应的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的LLVM IR值
 */
llvm::Value *SubroutineNode::codegen(CodegenContext &context) {
  std::vector<llvm::Type *> llvmTypes;
  std::vector<std::string> names;
  std::vector<std::shared_ptr<TypeNode>> types;
  for (auto child : params->children()) {
    auto decl = cast_node<ParamDeclNode>(child);
    types.push_back(decl->type);
    llvmTypes.push_back(decl->type->get_llvm_type(context));
    names.push_back(decl->name->name);
  }
  auto *func_type = llvm::FunctionType::get(return_type->get_llvm_type(context), llvmTypes, false);
  auto *func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, name->name, context.module.get());
  auto *block = llvm::BasicBlock::Create(context.module->getContext(), "entry", func);
  context.builder.SetInsertPoint(block);

  //将形参匹配到实参
  auto index = 0;
  for (auto &arg : func->args()) {
    std::string argName = names[index];
    auto &argType = types[index];
    context.symbolTable.addLocalSymbol(argName, argType);
    auto ptr = context.symbolTable.getLocalSymbol(argName)->get_llvmptr();
    context.builder.CreateStore(&arg, ptr);
    index += 1;
  }
  if (return_type->type != Type::VOID) {
    context.symbolTable.addLocalSymbol(name->name, return_type);
  }

  head_list->codegen(context);
  for (auto &stmt : children()) stmt->codegen(context);

  if (return_type->type == Type::VOID) {
    context.builder.CreateRetVoid();
  } else  //处理返回值
  {
    auto local = context.symbolTable.getLocalSymbol(name->name);  //根据Pascal的规则，对函数名的赋值即为返回值
    // auto *local = context.get_local(name->name);  //根据Pascal的规则，对函数名的赋值即为返回值
    auto *ret = context.builder.CreateLoad(local->get_llvmptr());
    context.builder.CreateRet(ret);
  }

  llvm::verifyFunction(*func);
  if (context.fpm) {
    context.fpm->run(*func);
  }

  context.symbolTable.resetLocals();  //清除局部信息 为下一个函数的生成做准备
  return nullptr;
}

/**
 * @brief 生成头部列表语义节点对应的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的LLVM IR值
 */
llvm::Value *HeadListNode::codegen(CodegenContext &context) {
  const_list->codegen(context);
  type_list->codegen(context);
  var_list->codegen(context);
  subroutine_list->codegen(context);
  return nullptr;
}

/**
 * @brief 生成子过程列表节点对应的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回生成的LLVM IR值
 */
llvm::Value *SubroutineListNode::codegen(CodegenContext &context) {
  for (auto &routine : children()) routine->codegen(context);
  return nullptr;
}

/* -------- identifier nodes -------- */
/**
 * @brief 获取标识符引用的指针
 * @param context 代码生成上下文
 * @return llvm::Value* 标识符引用的指针
 */
llvm::Value *IdentifierNode::get_ptr(CodegenContext &context) {
  auto value = context.symbolTable.getLocalSymbol(name);
  if (value == nullptr) value = context.symbolTable.getGlobalSymbol(name);
  if (value == nullptr) throw CodegenException("identifier not found: " + name);
  return value->get_llvmptr();
}

/**
 * @brief 获取数组元素的指针
 * @param context 代码生成上下文
 * @return llvm::Value* 数组元素的指针
 */
llvm::Value *ArrayRefNode::get_ptr(CodegenContext &context) {
  auto value = context.symbolTable.getLocalSymbol(name);
  if (value == nullptr) value = context.symbolTable.getGlobalSymbol(name);
  if (value == nullptr) throw CodegenException("identifier not found: " + name);
  return context.builder.CreateGEP(value->get_llvmptr(),
                                   std::vector<llvm::Value *>{context.builder.getInt32(0), get_index(context)});
}

/**
 * @brief 获取数组下标
 * @param context 代码生成上下文
 * @return llvm::Value* 返回数组下标
 */
llvm::Value *ArrayRefNode::get_index(CodegenContext &context) {
  auto value = context.symbolTable.getLocalSymbol(name);
  if (value == nullptr) value = context.symbolTable.getGlobalSymbol(name);
  if (value == nullptr) throw CodegenException("identifier not found: " + name);
  auto bound = cast_node<ArrayTypeNode>(value->typeNode)->bounds;
  if (!i) return context.builder.getInt32(index - bound.first);
  return context.builder.CreateSub(i->codegen(context), context.builder.getInt32(bound.first));
}

/**
 * @brief 获取记录结构体的指针
 * @param context 代码生成上下文
 * @return llvm::Value* 返回记录结构体的LLVM类型
 */
llvm::Value *StructRefNode::get_ptr(CodegenContext &context) {
  auto value = context.symbolTable.getLocalSymbol(name);
  if (value == nullptr) value = context.symbolTable.getGlobalSymbol(name);
  if (value == nullptr) throw CodegenException("identifier not found: " + name);
  return context.builder.CreateGEP(
      value->get_llvmptr(),
      std::vector<llvm::Value *>{context.builder.getInt32(0),
                                 context.builder.getInt32(cast_node<RecordTypeNode>(value->typeNode)->index[index])});
}
/**
 * @brief 获取标识符的LLVM类型
 * @param context 代码生成上下文
 * @return llvm::Type* 返回标识符的LLVM类型
 */
llvm::Type *IdentifierNode::get_llvmtype(CodegenContext &context) {
  auto value = context.symbolTable.getLocalSymbol(name);
  if (value == nullptr) value = context.symbolTable.getGlobalSymbol(name);
  if (value == nullptr) throw CodegenException("identifier not found: " + name);
  return value->get_llvmtype(context);
}

/**
 * @brief 根据标识符引用获取对应的 LLVM 值的指针，并使用指针创建一个 load 指令生成 LLVM 值
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *IdentifierNode::codegen(CodegenContext &context) { return context.builder.CreateLoad(get_ptr(context)); }
/**
 * @brief 根据数组引用获取对应的 LLVM 值的指针，并使用指针创建一个 load 指令生成 LLVM 值
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *ArrayRefNode::codegen(CodegenContext &context) { return context.builder.CreateLoad(get_ptr(context)); }
/**
 * @brief 根据记录引用获取对应的 LLVM 值的指针，并使用指针创建一个 load 指令生成 LLVM 值
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *StructRefNode::codegen(CodegenContext &context) { return context.builder.CreateLoad(get_ptr(context)); }
/* -------- stmt nodes -------- */
/**
 * @brief 生成赋值语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *AssignStmtNode::codegen(CodegenContext &context) {
  auto assignee = cast_node<LeftValueExprNode>(this->lhs);
  auto *lhs = assignee->get_ptr(context);
  auto *rhs = this->rhs->codegen(context);
  auto *lhs_type = lhs->getType()->getPointerElementType();
  auto *rhs_type = rhs->getType();
  if (lhs_type->isDoubleTy() && rhs_type->isIntegerTy(32)) {
    rhs = context.builder.CreateSIToFP(rhs, context.builder.getDoubleTy());
  } else if (!((lhs_type->isIntegerTy(1) && rhs_type->isIntegerTy(1)) ||
               (lhs_type->isIntegerTy(8) && rhs_type->isIntegerTy(8)) ||
               (lhs_type->isIntegerTy(32) && rhs_type->isIntegerTy(32)) ||
               (lhs_type->isDoubleTy() && rhs_type->isDoubleTy()) ||
               (lhs_type->isArrayTy() && lhs_type->getArrayElementType() == rhs_type))) {
    throw CodegenException("incompatible type in assignments: " + assignee->name);
  }
  context.builder.CreateStore(rhs, lhs);
  return nullptr;
}

/**
 * @brief 生成过程调用语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *ProcStmtNode::codegen(CodegenContext &context) {
  proc_call->codegen(context);
  return nullptr;
}
/**
 * @brief 生成if语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *IfStmtNode::codegen(CodegenContext &context) {
  llvm::Value *CondV = cond->codegen(context);
  CondV = context.builder.CreateICmpEQ(CondV, context.builder.getTrue(), "ifcond");
  llvm::Function *TheFunction = context.builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(context.module->getContext(), "then", TheFunction);
  llvm::BasicBlock *ElseBB = nullptr;
  if (else_stmt) ElseBB = llvm::BasicBlock::Create(context.module->getContext(), "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context.module->getContext(), "merge");
  if (else_stmt)
    context.builder.CreateCondBr(CondV, ThenBB, ElseBB);
  else
    context.builder.CreateCondBr(CondV, ThenBB, MergeBB);

  context.builder.SetInsertPoint(ThenBB);
  then_stmt->codegen(context);
  context.builder.CreateBr(MergeBB);

  if (else_stmt) {
    TheFunction->getBasicBlockList().push_back(ElseBB);
    context.builder.SetInsertPoint(ElseBB);
    else_stmt->codegen(context);
    context.builder.CreateBr(MergeBB);
  }

  TheFunction->getBasicBlockList().push_back(MergeBB);
  context.builder.SetInsertPoint(MergeBB);
  return nullptr;
}
/**
 * @brief 生成case语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *CaseStmtNode::codegen(CodegenContext &context) {
  llvm::Value *CondV = cond->codegen(context);
  llvm::Function *TheFunction = context.builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *DefaultBB = nullptr;
  if (default_stmt) DefaultBB = llvm::BasicBlock::Create(context.module->getContext(), "default", TheFunction);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context.module->getContext(), "merge", TheFunction);
  auto switch_case = context.builder.CreateSwitch(CondV, DefaultBB, body->children().size());
  for (auto branch : body->children()) {
    auto node = cast_node<CaseNode>(branch);
    auto BranchBB = llvm::BasicBlock::Create(context.module->getContext(), "branch", TheFunction);
    context.builder.SetInsertPoint(BranchBB);
    node->stmt->codegen(context);
    context.builder.CreateBr(MergeBB);
    //暂时只支持int, char
    if (is_a_ptr_of<IntegerNode>(node->cond)) {
      auto int_case = cast_node<IntegerNode>(node->cond);
      switch_case->addCase(context.builder.getInt32(int_case->val), BranchBB);
    } else if (is_a_ptr_of<CharNode>(node->cond)) {
      auto char_case = cast_node<CharNode>(node->cond);
      switch_case->addCase(context.builder.getInt32((int)char_case->val), BranchBB);
    }
  }

  if (default_stmt) {
    context.builder.SetInsertPoint(DefaultBB);
    default_stmt->codegen(context);
    context.builder.CreateBr(MergeBB);
  } else
    switch_case->setDefaultDest(MergeBB);

  context.builder.SetInsertPoint(MergeBB);
  return nullptr;
}
/**
 * @brief 将 LLVM 类型转换为字符串类型表示
 * @param context 代码生成上下文
 * @param type  LLVM 类型
 * @return std::string 字符串类型表示
 * @remarks 如果类型为 8 位整型，则返回 "char"；
 * 如果类型为 32 位整型，则返回 "integer"；
 * 如果类型为双精度浮点型，则返回 "double"；
 * 否则返回空字符串。
 */
std::string type2string(CodegenContext &context, llvm::Type *type) {
  if (type->isIntegerTy(8))
    return "char";
  else if (type->isIntegerTy(32))
    return "integer";
  else if (type->isDoubleTy())
    return "double";
  return "";
}
/**
 * @brief 生成循环语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *LoopStmtNode::codegen(CodegenContext &context) {
  if (type == LoopType::REPEAT) loop_stmt->codegen(context);
  llvm::Value *CondV = cond->codegen(context);
  llvm::Value *EndV = nullptr;
  if (type == LoopType::FOR || type == LoopType::FORDOWN) {
    if (CondV->getType() != i->get_ptr(context)->getType()->getPointerElementType())
      throw CodegenException("incompatible type in assignments: " + i->name);
    EndV = bound->codegen(context);
    if (EndV->getType() != CondV->getType())
      throw CodegenException("incompatible type in for-do: read " + type2string(context, EndV->getType()) +
                             ", expected " + type2string(context, CondV->getType()));
    context.builder.CreateStore(CondV, i->get_ptr(context));
    if (type == LoopType::FOR)
      CondV = context.builder.CreateICmpSGT(i->codegen(context), EndV);
    else
      CondV = context.builder.CreateICmpSLT(i->codegen(context), EndV);
  }
  llvm::Function *TheFunction = context.builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(context.module->getContext(), "loop", TheFunction);
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(context.module->getContext(), "end", TheFunction);
  if (type == LoopType::WHILE)
    context.builder.CreateCondBr(CondV, LoopBB, AfterBB);
  else
    context.builder.CreateCondBr(CondV, AfterBB, LoopBB);
  context.builder.SetInsertPoint(LoopBB);
  loop_stmt->codegen(context);

  if (type == LoopType::REPEAT || type == LoopType::WHILE) {
    CondV = cond->codegen(context);
  } else {
    llvm::Value *one = llvm::ConstantInt::get(EndV->getType(), 1);
    if (type == LoopType::FOR) {
      context.builder.CreateStore(context.builder.CreateAdd(i->codegen(context), one), i->get_ptr(context));
      CondV = context.builder.CreateICmpSGT(i->codegen(context), EndV);
    } else {
      context.builder.CreateStore(context.builder.CreateSub(i->codegen(context), one), i->get_ptr(context));
      CondV = context.builder.CreateICmpSLT(i->codegen(context), EndV);
    }
  }
  if (type == LoopType::WHILE)
    context.builder.CreateCondBr(CondV, LoopBB, AfterBB);
  else
    context.builder.CreateCondBr(CondV, AfterBB, LoopBB);
  context.builder.SetInsertPoint(AfterBB);
  return nullptr;
}

/**
 * @brief 生成复合语句的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *CompoundStmtNode::codegen(CodegenContext &context) {
  for (auto &child : children()) child->codegen(context);
  return nullptr;
}

/* -------- case node -------- */
/**
 * @brief 生成case节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *CaseNode::codegen(CodegenContext &context) { return nullptr; }

/* -------- const value nodes -------- */
/**
 * @brief 生成字符串常量的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回字符串常量对应的全局变量指针
 */
llvm::Value *StringNode::codegen(CodegenContext &context) { return context.builder.CreateGlobalStringPtr(val); }

/**
 * @brief 生成布尔值常量的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回布尔型常量对应的值
 */
llvm::Value *BoolenNode::codegen(CodegenContext &context) {
  return val == true ? context.builder.getTrue() : context.builder.getFalse();
}

/**
 * @brief 生成实数常量的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 返回实数常量对应的常量表达式指针
 */
llvm::Value *RealNode::codegen(CodegenContext &context) {
  auto *type = context.builder.getDoubleTy();
  return llvm::ConstantFP::get(type, val);
}

/**
 * @brief 生成整数常量的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *IntegerNode::codegen(CodegenContext &context) {
  auto *type = context.builder.getInt32Ty();
  return llvm::ConstantInt::getSigned(type, val);
}

/**
 * @brief 生成字符常量的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *CharNode::codegen(CodegenContext &context) { return context.builder.getInt8(static_cast<uint8_t>(val)); }

/* -------- decl nodes -------- */
/**
 * @brief 生成常量列表的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *ConstListNode::codegen(CodegenContext &context) {
  for (auto &child : children()) child->codegen(context);
  return nullptr;
}

/**
 * @brief 生成常量声明节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *ConstDeclNode::codegen(CodegenContext &context) {
  if (context.is_subroutine) {
    bool success = context.symbolTable.addLocalSymbol(name->name, value->type, true);
    if (!success) throw CodegenException("duplicate identifier in const section: " + name->name);
    auto local = context.symbolTable.getLocalSymbol(name->name);
    context.builder.CreateStore(value->codegen(context), local->get_llvmptr());
    return local->get_llvmptr();
  } else {
    if (is_a_ptr_of<StringNode>(value))  //符号表字符串的支持还有问题，先在这打个洞吧...
      return value->codegen(context);
    auto *constant = llvm::cast<llvm::Constant>(value->codegen(context));  //获得初值
    bool success = context.symbolTable.addGlobalSymbol(name->name, value->type, constant, true);
    return context.symbolTable.getGlobalSymbol(name->name)->get_llvmptr();
  }
}

/**
 * @brief 生成变量列表节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *VarListNode::codegen(CodegenContext &context) {
  for (auto &child : children()) child->codegen(context);
  return nullptr;
}

/**
 * @brief 生成变量声明节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *VarDeclNode::codegen(CodegenContext &context) {
  if (context.is_subroutine)  //如果是子过程 分配临时变量
  {
    bool success = context.symbolTable.addLocalSymbol(name->name, type);
    if (!success) throw CodegenException("duplicate identifier in var section: " + name->name);
    auto local = context.symbolTable.getLocalSymbol(name->name);
    return local->get_llvmptr();
  } else {
    bool success = context.symbolTable.addGlobalSymbol(name->name, type, nullptr);
    if (!success) throw CodegenException("duplicate global identifier in var sectrion: " + name->name);
    auto ptr = context.symbolTable.getGlobalSymbol(name->name)->get_llvmptr();
    return ptr;
  }
}

/**
 * @brief 生成类型列表语义节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *TypeListNode::codegen(CodegenContext &context) {
  for (auto &child : children()) child->codegen(context);
  return nullptr;
}

/**
 * @brief 生成类型定义节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *TypeDefNode::codegen(CodegenContext &context) {
  if (context.is_subroutine) {
    bool success = context.symbolTable.addLocalAlias(name->name, type);
    if (!success) throw CodegenException(fmt::format("duplicate local type alias: \"{}\"", name->name));
  } else {
    bool success = context.symbolTable.addGlobalAlias(name->name, type);
    if (!success) throw CodegenException(fmt::format("duplicate global type alias: \"{}\"", name->name));
  }
  return nullptr;
}

/* -------- expr nodes -------- */
/**
 * @brief 生成表达式相关节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值
 */
llvm::Value *BinopExprNode::codegen(CodegenContext &context) {
  auto *lhs = this->lhs->codegen(context);
  auto *rhs = this->rhs->codegen(context);
  if (lhs->getType()->isIntegerTy(1) && rhs->getType()->isIntegerTy(1)) {
    llvm::CmpInst::Predicate cmp;
    switch (op) {
      case BinaryOperator::GT:
        cmp = llvm::CmpInst::ICMP_SGT;
        break;
      case BinaryOperator::GE:
        cmp = llvm::CmpInst::ICMP_SGE;
        break;
      case BinaryOperator::LT:
        cmp = llvm::CmpInst::ICMP_SLT;
        break;
      case BinaryOperator::LE:
        cmp = llvm::CmpInst::ICMP_SLE;
        break;
      case BinaryOperator::EQ:
        cmp = llvm::CmpInst::ICMP_EQ;
        break;
      case BinaryOperator::NE:
        cmp = llvm::CmpInst::ICMP_NE;
        break;
      default:
        cmp = llvm::CmpInst::FCMP_FALSE;
    }
    if (cmp != llvm::CmpInst::FCMP_FALSE) return context.builder.CreateICmp(cmp, lhs, rhs);
    llvm::Instruction::BinaryOps binop;
    switch (op) {
      case BinaryOperator::AND:
        binop = llvm::Instruction::And;
        break;
      case BinaryOperator::OR:
        binop = llvm::Instruction::Or;
        break;
      case BinaryOperator::XOR:
        binop = llvm::Instruction::Xor;
        break;
      default:
        throw CodegenException("operator is invalid: boolean " + to_string(op) + " boolean");
    }
    return context.builder.CreateBinOp(binop, lhs, rhs);
  } else if (lhs->getType()->isIntegerTy(32) && rhs->getType()->isIntegerTy(32)) {
    llvm::CmpInst::Predicate cmp;
    switch (op) {
      case BinaryOperator::GT:
        cmp = llvm::CmpInst::ICMP_SGT;
        break;
      case BinaryOperator::GE:
        cmp = llvm::CmpInst::ICMP_SGE;
        break;
      case BinaryOperator::LT:
        cmp = llvm::CmpInst::ICMP_SLT;
        break;
      case BinaryOperator::LE:
        cmp = llvm::CmpInst::ICMP_SLE;
        break;
      case BinaryOperator::EQ:
        cmp = llvm::CmpInst::ICMP_EQ;
        break;
      case BinaryOperator::NE:
        cmp = llvm::CmpInst::ICMP_NE;
        break;
      default:
        cmp = llvm::CmpInst::FCMP_FALSE;
    }
    if (cmp != llvm::CmpInst::FCMP_FALSE) return context.builder.CreateICmp(cmp, lhs, rhs);
    llvm::Instruction::BinaryOps binop;
    switch (op) {
      case BinaryOperator::ADD:
        binop = llvm::Instruction::Add;
        break;
      case BinaryOperator::SUB:
        binop = llvm::Instruction::Sub;
        break;
      case BinaryOperator::MUL:
        binop = llvm::Instruction::Mul;
        break;
      case BinaryOperator::DIV:
        binop = llvm::Instruction::SDiv;
        break;
      case BinaryOperator::MOD:
        binop = llvm::Instruction::SRem;
        break;
      case BinaryOperator::AND:
        binop = llvm::Instruction::And;
        break;
      case BinaryOperator::OR:
        binop = llvm::Instruction::Or;
        break;
      case BinaryOperator::XOR:
        binop = llvm::Instruction::Xor;
        break;
      case BinaryOperator::TRUEDIV:
        lhs = context.builder.CreateSIToFP(lhs, context.builder.getDoubleTy());
        rhs = context.builder.CreateSIToFP(rhs, context.builder.getDoubleTy());
        binop = llvm::Instruction::FDiv;
        break;
      default:
        throw CodegenException("operator is invalid: integer " + to_string(op) + " integer");
    }
    return context.builder.CreateBinOp(binop, lhs, rhs);
  } else if (lhs->getType()->isDoubleTy() || rhs->getType()->isDoubleTy()) {
    if (lhs->getType()->isIntegerTy(32)) lhs = context.builder.CreateSIToFP(lhs, context.builder.getDoubleTy());
    if (rhs->getType()->isIntegerTy(32)) rhs = context.builder.CreateSIToFP(rhs, context.builder.getDoubleTy());
    llvm::CmpInst::Predicate cmp;
    switch (op) {
      case BinaryOperator::GT:
        cmp = llvm::CmpInst::FCMP_OGT;
        break;
      case BinaryOperator::GE:
        cmp = llvm::CmpInst::FCMP_OGE;
        break;
      case BinaryOperator::LT:
        cmp = llvm::CmpInst::FCMP_OLT;
        break;
      case BinaryOperator::LE:
        cmp = llvm::CmpInst::FCMP_OLE;
        break;
      case BinaryOperator::EQ:
        cmp = llvm::CmpInst::FCMP_OEQ;
        break;
      case BinaryOperator::NE:
        cmp = llvm::CmpInst::FCMP_ONE;
        break;
      default:
        cmp = llvm::CmpInst::FCMP_FALSE;
    }
    if (cmp != llvm::CmpInst::FCMP_FALSE) return context.builder.CreateFCmp(cmp, lhs, rhs);
    llvm::Instruction::BinaryOps binop;
    switch (op) {
      case BinaryOperator::ADD:
        binop = llvm::Instruction::FAdd;
        break;
      case BinaryOperator::SUB:
        binop = llvm::Instruction::FSub;
        break;
      case BinaryOperator::MUL:
        binop = llvm::Instruction::FMul;
        break;
      case BinaryOperator::TRUEDIV:
        binop = llvm::Instruction::FDiv;
        break;
      default:
        throw CodegenException("operator is invalid: real " + to_string(op) + " real");
    }
    return context.builder.CreateBinOp(binop, lhs, rhs);
  } else if (lhs->getType()->isIntegerTy(8) && rhs->getType()->isIntegerTy(8)) {
    llvm::CmpInst::Predicate cmp;
    switch (op) {
      case BinaryOperator::GT:
        cmp = llvm::CmpInst::ICMP_SGT;
        break;
      case BinaryOperator::GE:
        cmp = llvm::CmpInst::ICMP_SGE;
        break;
      case BinaryOperator::LT:
        cmp = llvm::CmpInst::ICMP_SLT;
        break;
      case BinaryOperator::LE:
        cmp = llvm::CmpInst::ICMP_SLE;
        break;
      case BinaryOperator::EQ:
        cmp = llvm::CmpInst::ICMP_EQ;
        break;
      case BinaryOperator::NE:
        cmp = llvm::CmpInst::ICMP_NE;
        break;
      default:
        throw CodegenException("operator is invalid: char " + to_string(op) + " char");
    }
    return context.builder.CreateICmp(cmp, lhs, rhs);
  } else {
    throw CodegenException("operator is invalid: " + to_string(op) + " between different types");
  }
}
/**
 * @brief 生成函数表达式节点的LLVM IR代码
 * @param context 代码生成上下文
 * @return llvm::Value* 生成的 LLVM 值表示函数调用的值的指针
 */
llvm::Value *FuncExprNode::codegen(CodegenContext &context) { return func_call->codegen(context); }
}  // namespace spc
