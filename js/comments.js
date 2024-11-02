const commentsContainer = document.querySelector(".comments");
if (commentsContainer) {
  let loadingTimeout = 0;
  let loadingIndicator = document.createElement("div");
  commentsContainer.appendChild(loadingIndicator);

  const intersectionObserver = new IntersectionObserver((entries, observer) => {
    if (entries[0].isIntersecting) {
      loadingIndicator.innerHTML = "<span>댓글을 로딩중입니다...</span>";
      loadingTimeout = setTimeout(() => {
        loadingIndicator.innerHTML =
          "<span>문제가 발생했습니다. 페이지를 새로고침 해주세요.</span>";
      }, 1000 * 5);
      observer.disconnect();
    }
  });
  intersectionObserver.observe(loadingIndicator);

  const mutationObserver = new MutationObserver((mutations, observer) => {
    mutations.forEach((mutation) => {
      if (mutation.target.classList.contains("utterances")) {
        clearTimeout(loadingTimeout);
        loadingIndicator.remove();
        observer.disconnect();
      }
    });
  });
  mutationObserver.observe(commentsContainer, {
    attributeFilter: ["style"],
    subtree: true,
  });
}
