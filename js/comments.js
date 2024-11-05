const commentsContainer = document.querySelector(".comments");
if (commentsContainer) {
  let loadingTimeout = 0;
  let loadingIndicator = document.createElement("p");
  commentsContainer.appendChild(loadingIndicator);

  const intersectionObserver = new IntersectionObserver((entries, observer) => {
    if (entries[0].isIntersecting) {
      loadingIndicator.innerText = "댓글을 로딩중입니다...";
      loadingTimeout = setTimeout(() => {
        loadingIndicator.innerText =
          "댓글을 로딩하는 중 문제가 발생했습니다. 페이지를 새로고침 해주세요.";
      }, 1000 * 7);
      observer.disconnect();
    }
  });
  intersectionObserver.observe(loadingIndicator);

  const mutationObserver = new MutationObserver((mutations, observer) => {
    mutations.forEach((mutation) => {
      if (mutation.target.classList.contains("utterances")) {
        clearTimeout(loadingTimeout);
        loadingIndicator.remove();
        intersectionObserver.disconnect();
        observer.disconnect();
      }
    });
  });
  mutationObserver.observe(commentsContainer, {
    attributeFilter: ["style"],
    subtree: true,
  });
}
