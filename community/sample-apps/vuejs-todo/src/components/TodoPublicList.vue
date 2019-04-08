<template>
  <div>
    <div class="todoListwrapper">
      <div class="loadMoreSection">
        You have 1 new todo
      </div>
      <ul>
        <TodoItem />
      </ul>
      <div class="loadMoreSection">
        Load Older Todos
      </div>
    </div>
    <TodoFilters />

  </div>
</template>

<script>
import TodoItem from "../components/TodoItem";
import TodoFilters from "../components/TodoFilters";
export default {
  components: {
    TodoItem, TodoFilters
  },
}
</script>
