import { CommonModule } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { RouterLink } from '@angular/router';
import { Subject, takeUntil } from 'rxjs';
import {
  BaiViet,
  BaoCao,
  LOAI_BAO_CAO_OPTIONS,
  TrangThaiBaoCao,
} from '../../../../models/community';
import { CommunityService } from '../../../../services/community.service';

@Component({
  selector: 'app-admin-community',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink],
  templateUrl: './admin-community.html',
  styleUrl: './admin-community.scss',
})
export class AdminCommunityComponent implements OnInit, OnDestroy {
  // Tab state
  activeTab: 'pending' | 'reports' = 'pending';

  // Pending posts
  pendingPosts: BaiViet[] = [];
  pendingLoading = false;
  pendingPage = 0;
  pendingTotalPages = 1;

  // Reports
  reports: BaoCao[] = [];
  reportsLoading = false;
  reportsPage = 0;
  reportsTotalPages = 1;
  reportStatus: TrangThaiBaoCao | 'ALL' = 'ALL';

  // Modal state
  showRejectModal = false;
  showReportModal = false;
  selectedPost: BaiViet | null = null;
  selectedReport: BaoCao | null = null;
  rejectReason = '';
  reportAction: 'approve' | 'reject' = 'approve';
  reportNote = '';

  // Processing
  processingIds: Set<number> = new Set();

  loaiBaoCaoOptions = LOAI_BAO_CAO_OPTIONS;

  private destroy$ = new Subject<void>();

  constructor(private communityService: CommunityService) {}

  ngOnInit(): void {
    this.loadPendingPosts();
    this.loadReports();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  // Tab switching
  switchTab(tab: 'pending' | 'reports'): void {
    this.activeTab = tab;
  }

  // Load pending posts
  loadPendingPosts(): void {
    this.pendingLoading = true;

    this.communityService
      .getPendingPosts(this.pendingPage, 10)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.pendingPosts = response.data.items;
            this.pendingTotalPages = response.data.totalPages;
          }
          this.pendingLoading = false;
        },
        error: () => {
          this.pendingLoading = false;
        },
      });
  }

  // Load reports
  loadReports(): void {
    this.reportsLoading = true;

    const status = this.reportStatus === 'ALL' ? undefined : this.reportStatus;

    this.communityService
      .getReports(this.reportsPage, 10, status)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.reports = response.data.items;
            this.reportsTotalPages = response.data.totalPages;
          }
          this.reportsLoading = false;
        },
        error: () => {
          this.reportsLoading = false;
        },
      });
  }

  // Approve post
  approvePost(post: BaiViet): void {
    if (this.processingIds.has(post.id)) return;

    this.processingIds.add(post.id);

    this.communityService
      .approvePost(post.id)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            this.pendingPosts = this.pendingPosts.filter((p) => p.id !== post.id);
          }
          this.processingIds.delete(post.id);
        },
        error: () => {
          this.processingIds.delete(post.id);
        },
      });
  }

  // Open reject modal
  openRejectModal(post: BaiViet): void {
    this.selectedPost = post;
    this.rejectReason = '';
    this.showRejectModal = true;
  }

  // Reject post
  rejectPost(): void {
    if (!this.selectedPost || !this.rejectReason.trim()) return;

    const postId = this.selectedPost.id;
    this.processingIds.add(postId);

    this.communityService
      .rejectPost(postId, this.rejectReason.trim())
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            this.pendingPosts = this.pendingPosts.filter((p) => p.id !== postId);
            this.closeRejectModal();
          }
          this.processingIds.delete(postId);
        },
        error: () => {
          this.processingIds.delete(postId);
        },
      });
  }

  closeRejectModal(): void {
    this.showRejectModal = false;
    this.selectedPost = null;
    this.rejectReason = '';
  }

  // Open report modal
  openReportModal(report: BaoCao): void {
    this.selectedReport = report;
    this.reportAction = 'approve';
    this.reportNote = '';
    this.showReportModal = true;
  }

  // Handle report
  handleReport(): void {
    if (!this.selectedReport) return;

    const reportId = this.selectedReport.id;
    this.processingIds.add(reportId);

    const dto = {
      chapNhan: this.reportAction === 'approve',
      ghiChu: this.reportNote.trim(),
    };

    this.communityService
      .handleReport(reportId, dto)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            this.loadReports();
            this.closeReportModal();
          }
          this.processingIds.delete(reportId);
        },
        error: () => {
          this.processingIds.delete(reportId);
        },
      });
  }

  closeReportModal(): void {
    this.showReportModal = false;
    this.selectedReport = null;
    this.reportNote = '';
  }

  // Pagination
  changePendingPage(page: number): void {
    if (page < 0 || page >= this.pendingTotalPages) return;
    this.pendingPage = page;
    this.loadPendingPosts();
  }

  changeReportsPage(page: number): void {
    if (page < 0 || page >= this.reportsTotalPages) return;
    this.reportsPage = page;
    this.loadReports();
  }

  onReportStatusChange(): void {
    this.reportsPage = 0;
    this.loadReports();
  }

  // Helpers
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

  getReportTypeLabel(loai: string): string {
    const option = this.loaiBaoCaoOptions.find((o) => o.value === loai);
    return option ? option.label : loai;
  }

  getReportStatusClass(status: string): string {
    const classes: Record<string, string> = {
      CHO_XU_LY: 'pending',
      DA_XU_LY: 'resolved',
      TU_CHOI: 'rejected',
    };
    return classes[status] || '';
  }

  getReportStatusLabel(status: string): string {
    const labels: Record<string, string> = {
      CHO_XU_LY: 'Chờ xử lý',
      DA_XU_LY: 'Đã xử lý',
      TU_CHOI: 'Từ chối',
    };
    return labels[status] || status;
  }

  formatDate(dateString: string): string {
    const date = new Date(dateString);
    return date.toLocaleDateString('vi-VN', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  }

  truncate(text: string, length: number): string {
    if (text.length <= length) return text;
    return text.substring(0, length) + '...';
  }
}
