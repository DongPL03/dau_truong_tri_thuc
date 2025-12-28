import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import {
  ApiResponse,
  BaiViet,
  BaoCao,
  BinhLuan,
  CreateBaiVietDTO,
  CreateBaoCaoDTO,
  CreateBinhLuanDTO,
  PageResponse,
  Tag,
  XuLyBaoCaoDTO,
} from '../models/community';
import { HttpUtilService } from './http.util.service';

@Injectable({
  providedIn: 'root',
})
export class CommunityService {
  private apiUrl = `${environment.apiBaseUrl}`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  // ============== TAG APIs ==============

  getAllTags(): Observable<ApiResponse<Tag[]>> {
    return this.http.get<ApiResponse<Tag[]>>(`${this.apiUrl}/tags`, {
      headers: this.httpUtil.createHeaders(),
    });
  }

  getPopularTags(limit: number = 10): Observable<ApiResponse<Tag[]>> {
    return this.http.get<ApiResponse<Tag[]>>(`${this.apiUrl}/tags/popular?limit=${limit}`, {
      headers: this.httpUtil.createHeaders(),
    });
  }

  getTagBySlug(slug: string): Observable<ApiResponse<Tag>> {
    return this.http.get<ApiResponse<Tag>>(`${this.apiUrl}/tags/slug/${slug}`, {
      headers: this.httpUtil.createHeaders(),
    });
  }

  // ============== POST APIs ==============

  getFeed(page: number = 0, limit: number = 10): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/feed?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  getHotPosts(
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/hot?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  getPostsByTag(
    tagId: number,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/tag/${tagId}?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  searchPosts(
    keyword: string,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/search?keyword=${encodeURIComponent(
        keyword
      )}&page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  getPostById(id: number): Observable<ApiResponse<BaiViet>> {
    return this.http.get<ApiResponse<BaiViet>>(`${this.apiUrl}/posts/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // Alias for getPostById
  getPost(id: number): Observable<ApiResponse<BaiViet>> {
    return this.getPostById(id);
  }

  getMyPosts(page: number = 0, limit: number = 10): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/my-posts?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  getSavedPosts(
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/saved?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  createPost(dto: CreateBaiVietDTO, images?: File[]): Observable<ApiResponse<BaiViet>> {
    const formData = new FormData();
    formData.append('data', new Blob([JSON.stringify(dto)], { type: 'application/json' }));

    if (images && images.length > 0) {
      images.forEach((image) => {
        formData.append('images', image);
      });
    }

    return this.http.post<ApiResponse<BaiViet>>(`${this.apiUrl}/posts`, formData, {
      headers: this.httpUtil.createAuthHeaders().delete('Content-Type'), // Let browser set multipart boundary
    });
  }

  updatePost(id: number, dto: CreateBaiVietDTO): Observable<ApiResponse<BaiViet>> {
    return this.http.put<ApiResponse<BaiViet>>(`${this.apiUrl}/posts/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  deletePost(id: number): Observable<ApiResponse<void>> {
    return this.http.delete<ApiResponse<void>>(`${this.apiUrl}/posts/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  toggleLikePost(id: number): Observable<ApiResponse<boolean>> {
    return this.http.post<ApiResponse<boolean>>(
      `${this.apiUrl}/posts/${id}/like`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  toggleSavePost(id: number): Observable<ApiResponse<boolean>> {
    return this.http.post<ApiResponse<boolean>>(
      `${this.apiUrl}/posts/${id}/save`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // Aliases for toggle methods
  toggleLike(id: number): Observable<ApiResponse<boolean>> {
    return this.toggleLikePost(id);
  }

  toggleSave(id: number): Observable<ApiResponse<boolean>> {
    return this.toggleSavePost(id);
  }

  // Upload image
  uploadImage(file: File): Observable<ApiResponse<string>> {
    const formData = new FormData();
    formData.append('file', file);

    return this.http.post<ApiResponse<string>>(`${this.apiUrl}/posts/upload-image`, formData, {
      headers: this.httpUtil.createAuthHeaders().delete('Content-Type'),
    });
  }

  // ============== COMMENT APIs ==============

  getCommentsByPost(
    postId: number,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BinhLuan>>> {
    return this.http.get<ApiResponse<PageResponse<BinhLuan>>>(
      `${this.apiUrl}/comments/post/${postId}?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  getReplies(
    commentId: number,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BinhLuan>>> {
    return this.http.get<ApiResponse<PageResponse<BinhLuan>>>(
      `${this.apiUrl}/comments/${commentId}/replies?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  createComment(dto: CreateBinhLuanDTO): Observable<ApiResponse<BinhLuan>> {
    return this.http.post<ApiResponse<BinhLuan>>(`${this.apiUrl}/comments`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  updateComment(id: number, dto: CreateBinhLuanDTO): Observable<ApiResponse<BinhLuan>> {
    return this.http.put<ApiResponse<BinhLuan>>(`${this.apiUrl}/comments/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  deleteComment(id: number): Observable<ApiResponse<void>> {
    return this.http.delete<ApiResponse<void>>(`${this.apiUrl}/comments/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  toggleLikeComment(id: number): Observable<ApiResponse<boolean>> {
    return this.http.post<ApiResponse<boolean>>(
      `${this.apiUrl}/comments/${id}/like`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // ============== REPORT APIs ==============

  reportPost(postId: number, dto: CreateBaoCaoDTO): Observable<ApiResponse<BaoCao>> {
    return this.http.post<ApiResponse<BaoCao>>(`${this.apiUrl}/reports/post/${postId}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  reportComment(commentId: number, dto: CreateBaoCaoDTO): Observable<ApiResponse<BaoCao>> {
    return this.http.post<ApiResponse<BaoCao>>(`${this.apiUrl}/reports/comment/${commentId}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getMyReports(
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaoCao>>> {
    return this.http.get<ApiResponse<PageResponse<BaoCao>>>(
      `${this.apiUrl}/reports/my-reports?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // ============== ADMIN APIs ==============

  adminGetAllPosts(
    status?: string,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    let params = `page=${page}&limit=${limit}`;
    if (status) params += `&status=${status}`;

    return this.http.get<ApiResponse<PageResponse<BaiViet>>>(
      `${this.apiUrl}/posts/admin/all?${params}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminApprovePost(id: number): Observable<ApiResponse<BaiViet>> {
    return this.http.post<ApiResponse<BaiViet>>(
      `${this.apiUrl}/posts/admin/${id}/approve`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // Alias
  approvePost(id: number): Observable<ApiResponse<BaiViet>> {
    return this.adminApprovePost(id);
  }

  adminRejectPost(id: number, reason?: string): Observable<ApiResponse<BaiViet>> {
    let url = `${this.apiUrl}/posts/admin/${id}/reject`;
    if (reason) url += `?reason=${encodeURIComponent(reason)}`;

    return this.http.post<ApiResponse<BaiViet>>(
      url,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // Alias
  rejectPost(id: number, reason?: string): Observable<ApiResponse<BaiViet>> {
    return this.adminRejectPost(id, reason);
  }

  adminHidePost(id: number): Observable<ApiResponse<BaiViet>> {
    return this.http.post<ApiResponse<BaiViet>>(
      `${this.apiUrl}/posts/admin/${id}/hide`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminTogglePinPost(id: number): Observable<ApiResponse<BaiViet>> {
    return this.http.post<ApiResponse<BaiViet>>(
      `${this.apiUrl}/posts/admin/${id}/pin`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminGetAllReports(
    status?: string,
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaoCao>>> {
    let params = `page=${page}&limit=${limit}`;
    if (status) params += `&status=${status}`;

    return this.http.get<ApiResponse<PageResponse<BaoCao>>>(
      `${this.apiUrl}/reports/admin/all?${params}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminGetPendingReports(
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaoCao>>> {
    return this.http.get<ApiResponse<PageResponse<BaoCao>>>(
      `${this.apiUrl}/reports/admin/pending?page=${page}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminResolveReport(id: number, dto: XuLyBaoCaoDTO): Observable<ApiResponse<BaoCao>> {
    return this.http.post<ApiResponse<BaoCao>>(`${this.apiUrl}/reports/admin/${id}/resolve`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  adminDismissReport(id: number): Observable<ApiResponse<BaoCao>> {
    return this.http.post<ApiResponse<BaoCao>>(
      `${this.apiUrl}/reports/admin/${id}/dismiss`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  adminGetReportStats(): Observable<ApiResponse<any>> {
    return this.http.get<ApiResponse<any>>(`${this.apiUrl}/reports/admin/stats`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // Aliases for admin methods
  getPendingPosts(
    page: number = 0,
    limit: number = 10
  ): Observable<ApiResponse<PageResponse<BaiViet>>> {
    return this.adminGetAllPosts('CHO_DUYET', page, limit);
  }

  getReports(
    page: number = 0,
    limit: number = 10,
    status?: string
  ): Observable<ApiResponse<PageResponse<BaoCao>>> {
    return this.adminGetAllReports(status, page, limit);
  }

  handleReport(id: number, dto: XuLyBaoCaoDTO): Observable<ApiResponse<BaoCao>> {
    return this.adminResolveReport(id, dto);
  }

  createReport(dto: CreateBaoCaoDTO): Observable<ApiResponse<BaoCao>> {
    if (dto.baiVietId) {
      return this.reportPost(dto.baiVietId, dto);
    } else if (dto.binhLuanId) {
      return this.reportComment(dto.binhLuanId, dto);
    }
    throw new Error('Must provide either baiVietId or binhLuanId');
  }

  // ============== ADMIN TAG APIs ==============

  adminCreateTag(dto: any): Observable<ApiResponse<Tag>> {
    return this.http.post<ApiResponse<Tag>>(`${this.apiUrl}/tags/admin`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  adminUpdateTag(id: number, dto: any): Observable<ApiResponse<Tag>> {
    return this.http.put<ApiResponse<Tag>>(`${this.apiUrl}/tags/admin/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  adminDeleteTag(id: number): Observable<ApiResponse<void>> {
    return this.http.delete<ApiResponse<void>>(`${this.apiUrl}/tags/admin/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  adminToggleTagVisibility(id: number): Observable<ApiResponse<Tag>> {
    return this.http.post<ApiResponse<Tag>>(
      `${this.apiUrl}/tags/admin/${id}/toggle-visibility`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }
}
