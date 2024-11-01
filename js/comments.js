const observer = new MutationObserver(handleMutations);
const commentsContainer = document.querySelector(".comments");

if (commentsContainer) {
  observer.observe(commentsContainer, {
    childList: true,
    attributeFilter: ["style"],
    subtree: true,
  });
}

function handleMutations(mutations) {
  mutations.forEach((mutation) => {
    if (mutation.type === "childList") {
      handleChildListMutation(mutation);
    } else if (mutation.type === "attributes") {
      handleAttributeMutation(mutation);
    }
  });
}

function handleChildListMutation(mutation) {
  mutation.addedNodes.forEach((node) => {
    if (node instanceof HTMLElement && node.classList.contains("utterances")) {
      attachLoadingIndicator(node);
    }
  });
}

function attachLoadingIndicator(node) {
  const loading = document.createElement("div");
  loading.className = "comments-loading";
  loading.innerHTML = "<span>댓글을 로딩중입니다...</span>";
  node.parentElement.appendChild(loading);
  setTimeout(() => {
    loading.innerHTML =
      "<span>문제가 발생했습니다. 페이지를 새로고침 해주세요.</span>";
  }, 1000 * 15);
}

function handleAttributeMutation(mutation) {
  const target = mutation.target;
  if (
    target instanceof HTMLElement &&
    target.classList.contains("utterances")
  ) {
    removeLoadingIndicator(target);
    observer.disconnect();
  }
}

function removeLoadingIndicator(target) {
  const loadingIndicator =
    target.parentElement.querySelector(".comments-loading");
  if (loadingIndicator) {
    loadingIndicator.remove();
  }
}
