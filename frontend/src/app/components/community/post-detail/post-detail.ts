import {CommonModule} from '@angular/common';
import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {RouterLink} from '@angular/router';
import {NgbDropdownModule} from '@ng-bootstrap/ng-bootstrap';
import {Subject, takeUntil} from 'rxjs';
import {environment} from '../../../environments/environment';
import {BaiViet, BinhLuan, BinhLuanDTO, LOAI_BAO_CAO_OPTIONS, LoaiBaoCao,} from '../../../models/community';
import {UserResponse} from '../../../responses/nguoidung/user-response';
import {CommunityService} from '../../../services/community.service';
import {Base} from '../../base/base';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-post-detail',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink, NgbDropdownModule],
  templateUrl: './post-detail.html',
  styleUrl: './post-detail.scss',
})
export class PostDetailComponent extends Base implements OnInit, OnDestroy {
  post: BaiViet | null = null;
  comments: BinhLuan[] = [];
  relatedPosts: BaiViet[] = [];

  isLoading = true;
  isSubmittingComment = false;

  // Comment form
  newComment = '';
  replyingTo: number | null = null;
  replyContent = '';

  // Edit comment
  editingComment: number | null = null;
  editContent = '';

  // Menu state
  showPostMenu = false;
  activeCommentMenu: number | null = null;

  // Report modal
  showReportModal = false;
  reportTarget: 'post' | 'comment' = 'post';
  reportTargetId: number = 0;
  selectedReportType: LoaiBaoCao | null = null;
  reportDetail = '';
  reportOptions = LOAI_BAO_CAO_OPTIONS;
  isSubmittingReport = false;

  // Current user info
  currentUserAvatar = '';
  user?: UserResponse | null = null;
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/users/profile-images/';

  private destroy$ = new Subject<void>();

  constructor(private communityService: CommunityService) {
    super();
  }

  getAvatarUrl(url: string | undefined): string {
    if (!url) return 'assets/images/default-avatar.png';
    if (url.startsWith('http')) return url;
    return `${environment.apiBaseUrl}/users/profile-images/${url}`;
  }

  ngOnInit(): void {
    this.route.params.pipe(takeUntil(this.destroy$)).subscribe((params) => {
      const postId = +params['id'];
      if (postId) {
        this.loadPost(postId);
      }
    });
    this.loadUserInfo();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  private loadUserInfo(): void {
    this.user = this.userService.getUserResponseFromLocalStorage();
    if (this.user?.avatar_url) {
      this.currentUserAvatar = this.imageBaseUrl + this.user.avatar_url;
    }
  }

  loadPost(postId: number): void {
    this.isLoading = true;

    this.communityService
      .getPost(postId)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.post = response.data;
            this.loadComments(postId);
            if (this.post.tags?.length) {
              this.loadRelatedPosts(this.post.tags[0].id);
            }
          }
          this.isLoading = false;
        },
        error: () => {
          this.isLoading = false;
          this.router.navigate(['/community']);
        },
      });
  }

  loadComments(postId: number): void {
    this.communityService
      .getCommentsByPost(postId)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.comments = response.data.items || [];
          }
        },
      });
  }

  loadRelatedPosts(tagId: number): void {
    this.communityService
      .getPostsByTag(tagId, 0, 5)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.relatedPosts = response.data.items
              .filter((p) => p.id !== this.post?.id)
              .slice(0, 4);
          }
        },
      });
  }

  // Like & Save
  toggleLike(): void {
    if (!this.post) return;

    this.communityService
      .toggleLike(this.post.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && this.post) {
            this.post.daThich = !this.post.daThich;
            this.post.soLuotThich += this.post.daThich ? 1 : -1;
          }
        },
      });
  }

  toggleSave(): void {
    if (!this.post) return;

    this.communityService
      .toggleSave(this.post.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && this.post) {
            this.post.daLuu = !this.post.daLuu;
          }
        },
      });
  }

  // Comment actions
  submitComment(): void {
    if (!this.newComment.trim() || !this.post) return;

    const dto: BinhLuanDTO = {
      baiVietId: this.post.id,
      noiDung: this.newComment.trim(),
    };

    this.isSubmittingComment = true;

    this.communityService
      .createComment(dto)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          this.isSubmittingComment = false;
          console.log(response);
          if (response.status === 'CREATED' && response.data) {
            // Reload only comments - much lighter than loadPost
            this.loadComments(this.post!.id);
            if (this.post) {
              this.post.soLuotBinhLuan++;
            }
            this.newComment = '';
          }
        },
        error: () => {
          this.isSubmittingComment = false;
        },
      });
  }

  startReply(commentId: number): void {
    this.replyingTo = commentId;
    this.replyContent = '';
    this.editingComment = null;
  }

  cancelReply(): void {
    this.replyingTo = null;
    this.replyContent = '';
  }

  submitReply(): void {
    if (!this.replyContent.trim() || !this.replyingTo || !this.post) return;

    const dto: BinhLuanDTO = {
      baiVietId: this.post.id,
      noiDung: this.replyContent.trim(),
      binhLuanChaId: this.replyingTo,
    };

    this.communityService
      .createComment(dto)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            // Reload comments from server to get full data including reply
            this.loadComments(this.post!.id);
            if (this.post) {
              this.post.soLuotBinhLuan++;
            }
            this.cancelReply();
          }
        },
      });
  }

  startEditComment(comment: BinhLuan): void {
    this.editingComment = comment.id;
    this.editContent = comment.noiDung;
    this.replyingTo = null;
    this.activeCommentMenu = null;
  }

  cancelEditComment(): void {
    this.editingComment = null;
    this.editContent = '';
  }

  saveEditComment(comment: BinhLuan): void {
    if (!this.editContent.trim() || !this.post) return;

    const updateDto = {
      baiVietId: this.post.id,
      noiDung: this.editContent.trim(),
    };

    this.communityService
      .updateComment(comment.id, updateDto)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            comment.noiDung = this.editContent.trim();
            comment.daSua = true;
            this.cancelEditComment();
          }
        },
      });
  }

  deleteComment(comment: BinhLuan, parentId?: number): void {
    if (!confirm('Bạn có chắc muốn xóa bình luận này?')) return;

    this.communityService
      .deleteComment(comment.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            if (parentId) {
              const parent = this.comments.find((c) => c.id === parentId);
              if (parent?.binhLuanCon) {
                parent.binhLuanCon = parent.binhLuanCon.filter((c) => c.id !== comment.id);
              }
            } else {
              const deletedComment = this.comments.find((c) => c.id === comment.id);
              const replyCount = deletedComment?.binhLuanCon?.length || 0;
              this.comments = this.comments.filter((c) => c.id !== comment.id);
              if (this.post) {
                this.post.soLuotBinhLuan -= 1 + replyCount;
              }
            }
          }
        },
      });
  }

  toggleLikeComment(comment: BinhLuan): void {
    this.communityService
      .toggleLikeComment(comment.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            comment.daThich = !comment.daThich;
            comment.soLuotThich += comment.daThich ? 1 : -1;
          }
        },
      });
  }

  toggleCommentMenu(commentId: number): void {
    this.activeCommentMenu = this.activeCommentMenu === commentId ? null : commentId;
  }

  // Post actions
  deletePost(): void {
    if (!this.post || !confirm('Bạn có chắc muốn xóa bài viết này?')) return;

    this.communityService
      .deletePost(this.post.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            this.router.navigate(['/community']);
          }
        },
      });
  }

  sharePost(): void {
    if (!this.post) return;

    const url = window.location.href;

    if (navigator.share) {
      navigator.share({
        title: this.post.tieuDe,
        url: url,
      });
    } else {
      navigator.clipboard.writeText(url);
      alert('Đã sao chép liên kết!');
    }
  }

  // Report
  openReportModal(target: 'post' | 'comment', targetId: number): void {
    this.reportTarget = target;
    this.reportTargetId = targetId;
    this.showReportModal = true;
    this.selectedReportType = null;
    this.reportDetail = '';
    this.showPostMenu = false;
    this.activeCommentMenu = null;
  }

  closeReportModal(): void {
    this.showReportModal = false;
    this.selectedReportType = null;
    this.reportDetail = '';
  }

  submitReport(): void {
    if (!this.selectedReportType) return;

    this.isSubmittingReport = true;

    const dto: any = {
      loai: this.selectedReportType,
      chiTiet: this.reportDetail || null,
    };

    if (this.reportTarget === 'post') {
      dto.baiVietId = this.reportTargetId;
    } else {
      dto.binhLuanId = this.reportTargetId;
    }

    this.communityService
      .createReport(dto)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'CREATED') {
            this.closeReportModal();
            Swal.fire({
              icon: 'success',
              title: 'Báo cáo đã được gửi',
              text: 'Cảm ơn bạn đã giúp chúng tôi giữ cộng đồng an toàn và lành mạnh.',
            }).then(r => {
            });
          }
          this.isSubmittingReport = false;
        },
        error: (err) => {
          Swal.fire({
            icon: 'error',
            title: 'Lỗi khi gửi báo cáo',
            text: err.error?.message || 'Đã có lỗi xảy ra. Vui lòng thử lại sau.',
          }).then(r => {
          });
          this.isSubmittingReport = false;
        },
      });
  }

  // Helpers
  getTypeColor(loai: string): string {
    const colors: Record<string, string> = {
      THAO_LUAN: '#6366f1',
      CAU_HOI: '#f59e0b',
      CHIA_SE: '#10b981',
      HUONG_DAN: '#3b82f6',
      THONG_BAO: '#ef4444',
    };
    return colors[loai] || '#6b7280';
  }

  getTypeLabel(loai: string): string {
    const labels: Record<string, string> = {
      THAO_LUAN: 'Thảo luận',
      CAU_HOI: 'Câu hỏi',
      CHIA_SE: 'Chia sẻ',
      HUONG_DAN: 'Hướng dẫn',
      THONG_BAO: 'Thông báo',
    };
    return labels[loai] || loai;
  }

  getTypeIcon(loai: string): string {
    const icons: Record<string, string> = {
      THAO_LUAN: 'fa-comments',
      CAU_HOI: 'fa-circle-question',
      CHIA_SE: 'fa-share-nodes',
      HUONG_DAN: 'fa-book-open',
      THONG_BAO: 'fa-bullhorn',
    };
    return icons[loai] || 'fa-file';
  }

  formatDate(dateString: string): string {
    const date = new Date(dateString);
    const now = new Date();
    const diff = now.getTime() - date.getTime();

    const minutes = Math.floor(diff / 60000);
    const hours = Math.floor(diff / 3600000);
    const days = Math.floor(diff / 86400000);

    if (minutes < 1) return 'Vừa xong';
    if (minutes < 60) return `${minutes} phút trước`;
    if (hours < 24) return `${hours} giờ trước`;
    if (days < 7) return `${days} ngày trước`;

    return date.toLocaleDateString('vi-VN', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
    });
  }
}
