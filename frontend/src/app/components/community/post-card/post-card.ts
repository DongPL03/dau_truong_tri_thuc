import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { environment } from '../../../environments/environment';
import { BaiViet, LOAI_BAI_VIET_OPTIONS, LOAI_BAO_CAO_OPTIONS } from '../../../models/community';
import { CommunityService } from '../../../services/community.service';
import { TokenService } from '../../../services/token.service';

@Component({
  selector: 'app-post-card',
  standalone: true,
  imports: [CommonModule, RouterModule, FormsModule],
  templateUrl: './post-card.html',
  styleUrls: ['./post-card.scss'],
})
export class PostCardComponent {
  @Input() post!: BaiViet;
  @Input() showFullContent = false;
  @Output() deleted = new EventEmitter<number>();
  @Output() updated = new EventEmitter<BaiViet>();

  showMenu = false;
  showReportModal = false;
  reportLoading = false;
  selectedReportType = '';
  reportDetail = '';

  loaiBaoCaoOptions = LOAI_BAO_CAO_OPTIONS;

  constructor(private communityService: CommunityService, private tokenService: TokenService) {}

  get author() {
    return this.post.nguoiDang || { id: 0, ten: 'Người dùng', anhDaiDien: '', capDo: 1 };
  }

  get isOwner(): boolean {
    return this.post.laCuaToi;
  }

  get postTypeInfo() {
    return (
      LOAI_BAI_VIET_OPTIONS.find((t) => t.value === this.post.loai) || LOAI_BAI_VIET_OPTIONS[0]
    );
  }

  get contentPreview(): string {
    if (this.showFullContent) return this.post.noiDung;
    // Strip HTML and limit to 200 chars
    const text = this.post.noiDung.replace(/<[^>]*>/g, '');
    return text.length > 200 ? text.substring(0, 200) + '...' : text;
  }

  get timeAgo(): string {
    if (!this.post.ngayTao) return '';

    const now = new Date();
    const created = new Date(this.post.ngayTao);

    if (isNaN(created.getTime())) return '';

    const diff = Math.floor((now.getTime() - created.getTime()) / 1000);

    if (diff < 60) return 'Vừa xong';
    if (diff < 3600) return `${Math.floor(diff / 60)} phút trước`;
    if (diff < 86400) return `${Math.floor(diff / 3600)} giờ trước`;
    if (diff < 604800) return `${Math.floor(diff / 86400)} ngày trước`;

    return created.toLocaleDateString('vi-VN');
  }

  getImageUrl(path: string): string {
    if (path.startsWith('http')) return path;
    return `${environment.apiBaseUrl}/uploads/images/${path}`;
  }

  getAvatarUrl(url: string | undefined): string {
    if (!url) return 'assets/images/default-avatar.png';
    if (url.startsWith('http')) return url;
    return `${environment.apiBaseUrl}/users/profile-images/${url}`;
  }

  toggleLike(): void {
    this.communityService.toggleLikePost(this.post.id).subscribe({
      next: (res) => {
        this.post.daThich = res.data;
        this.post.soLuotThich += res.data ? 1 : -1;
        this.updated.emit(this.post);
      },
      error: (err) => console.error('Error toggling like:', err),
    });
  }

  toggleSave(): void {
    this.communityService.toggleSavePost(this.post.id).subscribe({
      next: (res) => {
        this.post.daLuu = res.data;
        this.updated.emit(this.post);
      },
      error: (err) => console.error('Error toggling save:', err),
    });
  }

  deletePost(): void {
    if (!confirm('Bạn có chắc muốn xóa bài viết này?')) return;

    this.communityService.deletePost(this.post.id).subscribe({
      next: () => {
        this.deleted.emit(this.post.id);
        this.showMenu = false;
      },
      error: (err) => console.error('Error deleting post:', err),
    });
  }

  openReportModal(): void {
    this.showReportModal = true;
    this.showMenu = false;
  }

  submitReport(): void {
    if (!this.selectedReportType) return;

    this.reportLoading = true;
    this.communityService
      .reportPost(this.post.id, {
        loai: this.selectedReportType as any,
        chiTiet: this.reportDetail,
      })
      .subscribe({
        next: () => {
          alert('Đã gửi báo cáo thành công!');
          this.closeReportModal();
        },
        error: (err) => {
          console.error('Error reporting:', err);
          alert('Không thể gửi báo cáo. Vui lòng thử lại.');
          this.reportLoading = false;
        },
      });
  }

  closeReportModal(): void {
    this.showReportModal = false;
    this.selectedReportType = '';
    this.reportDetail = '';
    this.reportLoading = false;
  }

  sharePost(): void {
    const url = `${window.location.origin}/community/${this.post.id}`;
    if (navigator.share) {
      navigator.share({
        title: this.post.tieuDe,
        url: url,
      });
    } else {
      navigator.clipboard.writeText(url);
      alert('Đã sao chép link bài viết!');
    }
    this.showMenu = false;
  }
}
