import { CommonModule } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { debounceTime, distinctUntilChanged, Subject, takeUntil } from 'rxjs';
import { BaiViet, LOAI_BAI_VIET_OPTIONS, Tag } from '../../../models/community';
import { CommunityService } from '../../../services/community.service';
import { FriendsSidebarComponent } from '../../shared/friends-sidebar/friends-sidebar';
import { RecentChatsComponent } from '../../shared/recent-chats/recent-chats';
import { PostCardComponent } from '../post-card/post-card';

@Component({
  selector: 'app-community-feed',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    FormsModule,
    PostCardComponent,
    FriendsSidebarComponent,
    RecentChatsComponent,
  ],
  templateUrl: './community-feed.html',
  styleUrls: ['./community-feed.scss'],
})
export class CommunityFeedComponent implements OnInit, OnDestroy {
  private destroy$ = new Subject<void>();

  posts: BaiViet[] = [];
  tags: Tag[] = [];
  popularTags: Tag[] = [];

  // Pagination
  currentPage = 0;
  totalPages = 0;
  totalItems = 0;
  pageSize = 10;

  // Filters
  activeTab: 'feed' | 'hot' | 'saved' | 'my-posts' = 'feed';
  selectedTagId: number | null = null;
  searchKeyword = '';
  searchSubject = new Subject<string>();

  // State
  loading = false;
  error: string | null = null;

  loaiBaiVietOptions = LOAI_BAI_VIET_OPTIONS;

  constructor(
    private communityService: CommunityService,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadTags();
    this.loadPopularTags();

    // Setup search debounce
    this.searchSubject
      .pipe(takeUntil(this.destroy$), debounceTime(500), distinctUntilChanged())
      .subscribe((keyword) => {
        this.searchKeyword = keyword;
        this.currentPage = 0;
        this.loadPosts();
      });

    // Check route params
    this.route.queryParams.pipe(takeUntil(this.destroy$)).subscribe((params) => {
      if (params['tag']) {
        this.selectedTagId = +params['tag'];
      }
      if (params['tab']) {
        this.activeTab = params['tab'];
      }
      this.loadPosts();
    });
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  loadTags(): void {
    this.communityService.getAllTags().subscribe({
      next: (res) => {
        this.tags = res.data;
      },
      error: (err) => console.error('Error loading tags:', err),
    });
  }

  loadPopularTags(): void {
    this.communityService.getPopularTags(8).subscribe({
      next: (res) => {
        this.popularTags = res.data;
      },
      error: (err) => console.error('Error loading popular tags:', err),
    });
  }

  loadPosts(): void {
    this.loading = true;
    this.error = null;

    let request$;

    if (this.searchKeyword) {
      request$ = this.communityService.searchPosts(
        this.searchKeyword,
        this.currentPage,
        this.pageSize
      );
    } else if (this.selectedTagId) {
      request$ = this.communityService.getPostsByTag(
        this.selectedTagId,
        this.currentPage,
        this.pageSize
      );
    } else {
      switch (this.activeTab) {
        case 'hot':
          request$ = this.communityService.getHotPosts(this.currentPage, this.pageSize);
          break;
        case 'saved':
          request$ = this.communityService.getSavedPosts(this.currentPage, this.pageSize);
          break;
        case 'my-posts':
          request$ = this.communityService.getMyPosts(this.currentPage, this.pageSize);
          break;
        default:
          request$ = this.communityService.getFeed(this.currentPage, this.pageSize);
      }
    }

    request$.pipe(takeUntil(this.destroy$)).subscribe({
      next: (res) => {
        this.posts = res.data.items;
        this.totalPages = res.data.totalPages;
        this.totalItems = res.data.totalItems;
        this.loading = false;
      },
      error: (err) => {
        this.error = 'Không thể tải bài viết. Vui lòng thử lại.';
        this.loading = false;
        console.error('Error loading posts:', err);
      },
    });
  }

  onTabChange(tab: 'feed' | 'hot' | 'saved' | 'my-posts'): void {
    this.activeTab = tab;
    this.selectedTagId = null;
    this.searchKeyword = '';
    this.currentPage = 0;
    this.router.navigate([], {
      queryParams: { tab },
      queryParamsHandling: 'merge',
    });
    this.loadPosts();
  }

  onTagSelect(tagId: number | null): void {
    this.selectedTagId = tagId;
    this.currentPage = 0;
    this.router.navigate([], {
      queryParams: { tag: tagId },
      queryParamsHandling: 'merge',
    });
    this.loadPosts();
  }

  onSearch(keyword: string): void {
    this.searchSubject.next(keyword);
  }

  onPageChange(page: number): void {
    this.currentPage = page;
    this.loadPosts();
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }

  onPostDeleted(postId: number): void {
    this.posts = this.posts.filter((p) => p.id !== postId);
    this.totalItems--;
  }

  onPostUpdated(post: BaiViet): void {
    const index = this.posts.findIndex((p) => p.id === post.id);
    if (index !== -1) {
      this.posts[index] = post;
    }
  }

  getTagById(tagId: number): Tag | undefined {
    return this.tags.find((t) => t.id === tagId);
  }

  get pages(): number[] {
    const pages: number[] = [];
    const start = Math.max(0, this.currentPage - 2);
    const end = Math.min(this.totalPages - 1, this.currentPage + 2);

    for (let i = start; i <= end; i++) {
      pages.push(i);
    }
    return pages;
  }
}
