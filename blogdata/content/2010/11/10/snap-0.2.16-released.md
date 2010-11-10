| title: Announcing: Snap Framework v0.2.16
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-11-10T21:48:00+0100
| updated: 2010-11-10T21:48:00+0100
| summary: Release notes for version 0.2.16 of the Snap Framework: a fix for a critical security vulnerability

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.16. **This release
fixes a critical security vulnerability and all users of Snap should upgrade
immediately.**

Changes since 0.2.15
=====================

We're not entirely sure how we didn't catch this, but a commit made back in
April of this year caused our fileServe code to have a critical vulnerability
which allowed it to serve any file on the disk, including
`/etc/passwd`. Obviously if you are using Snap's file serving code, you should
upgrade to Snap 0.2.16 **immediately**.

Thanks,

--The Snap team
